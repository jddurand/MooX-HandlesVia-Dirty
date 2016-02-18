use strict;
use warnings FATAL => 'all';

package MooX::HandlesVia::Dirty;

# ABSTRACT: A MooX::HandlesVia fast version, with no check, inserting code references

# VERSION

# AUTHORITY

use List::Util qw//;
use List::MoreUtils qw//;
use Moo qw//;
use Moo::Role qw//;
use Scalar::Util qw//;
use Types::Standard -all;
use Type::Utils -all;

my $Handles_Via = Enum[qw/Array Hash String Number Bool Code/];
my $Handles     = HashRef[Str];

#
# import() copied from MooX::HandlesVia 0.001008
#
sub import {
  my $class = shift;

  my %myOpts = (trace => 0, replace => 0, target => caller);
  if (@_) {
    my %args = @_;
    foreach (qw/trace replace/) {
      $myOpts{$_} = $args{"-$_"} if exists $args{"-$_"}
    }
  }

  if (my $has = $myOpts{target}->can('has')) {
    $myOpts{install_tracked} = Moo::Role->is_role($myOpts{target}) ? \&Moo::Role::_install_tracked : \&Moo::_install_tracked;
    my $newsub = sub {
        $has->(_process_has(\%myOpts, @_));
    };
    $myOpts{install_tracked}->($myOpts{target}, 'has', $newsub);
  }
}

my %_ALIAS2ALIAS = (
                  inc    => '++x',
                  dec    => '--x',
                  add    => '+=',
                  sub    => '-=',
                  div    => '/=',
                  mul    => '*=',
                  append => '.=',
                  mod    => '%=',
                  set    => '=',
                  match  => '=~',
                  get    => '',
                 );

sub _process_has {
  my $myOpts = shift;
  my ($name, %opts) = @_;

  my $handles_via = $myOpts->{replace} ? delete($opts{handles_via}) : delete($opts{handles_via_dirty});
  my $handles     = $myOpts->{replace} ?        $opts{handles}      :        $opts{handles_dirty};

  if ($Handles_Via->check($handles_via)) {
    if ($Handles->check($handles)) {
      #
      # Re-alias eventually
      #
      foreach my $key (keys %{$handles}) {
        my $alias = $handles->{$key};
        if (grep { $alias eq $_ } keys %_ALIAS2ALIAS) {
          $handles->{$key} = $_ALIAS2ALIAS{$alias}
        }
      }
      #
      # Generate stubs.
      # This will leave handles unsatisfied in there
      #
      if    ($handles_via eq 'Array')  { _handles_via_Array ($myOpts, $name, $handles) }
      elsif ($handles_via eq 'Hash')   { _handles_via_Hash  ($myOpts, $name, $handles) }
      elsif ($handles_via eq 'String') { _handles_via_String($myOpts, $name, $handles) }
      elsif ($handles_via eq 'Number') { _handles_via_Number($myOpts, $name, $handles) }
      elsif ($handles_via eq 'Bool')   { _handles_via_Bool  ($myOpts, $name, $handles) }
      elsif ($handles_via eq 'Code')   { _handles_via_Code  ($myOpts, $name, $handles) }
    }
    #
    # Clone handles not handled -;
    #
    my %handles_clone = %{$handles};
    #
    # Copy of a part of MooX::HandlesVia::import
    #
    # install our support for moose upgrading of class/role
    # we deleted the handles_via key above, but install it as a native trait
    #
    my $inflator = $opts{moosify};
    $opts{moosify} = sub {
      #
      my ($spec) = @_;

      $spec->{handles} = \%handles_clone;
      $spec->{handles_via} = $handles_via;

      # pass through if needed
      $inflator->($spec) if ref($inflator) eq 'CODE';
    };
  }
  #
  # Next 'has' round
  #
  return ($name, %opts)
}

my $_FIRST  = $[;
my $_SECOND = $[+1;
my $_THIRD  = $[+2;
my $_FOURTH = $[+3;
my $firstArgument           = "\$_[$_FIRST]";
my $secondArgument          = "\$_[$_SECOND]";
my $thirdArgument           = "\$_[$_THIRD]";
my $fourthArgument          = "\$_[$_FOURTH]";
my $stackFromSecondArgument = "\@_[$_SECOND..\$#_]";
my $stackFromThirdArgument  = "\@_[$_THIRD..\$#_]";
my $stackFromFourthArgument = "\@_[$_FOURTH..\$#_]";
my $nbArguments             = "CORE::scalar(\@_)";
my $_flatten_deep           = join('::', __PACKAGE__, '_flatten_deep');

sub _handles_via_Array {
  my ($myOpts, $name, $handles) = @_;

  my $accessor    = "{$name}";
  my $member      = $firstArgument . '->' . $accessor;
  my $memberValue = $member . '->' . "[$secondArgument]";
  my $derefMember = "\@{$member}";

  my ($str, $coderef);
  while (my ($stubname, $alias) = each %{$handles}) {
    ($str, $coderef) = (undef, undef);
    if    ($alias eq 'count')         { $str = "sub { return CORE::scalar($derefMember) }" }
    elsif ($alias eq 'is_empty')      { $str = "sub { return CORE::scalar($derefMember) ? 0 : 1 }" }
    elsif ($alias eq 'pop')           { $str = "sub { return CORE::pop($derefMember) }" }
    elsif ($alias eq 'push')          { $str = "sub { return CORE::push($derefMember, $stackFromSecondArgument) }" }
    elsif ($alias eq 'shift')         { $str = "sub { return CORE::shift($derefMember) }" }
    elsif ($alias eq 'unshift')       { $str = "sub { return CORE::unshift($derefMember, $stackFromSecondArgument) }" }
    elsif ($alias eq 'clear')         { $str = "sub { return $derefMember = () }" }
    elsif ($alias eq 'first')         { $str = "sub { return &List::Util::first($secondArgument, $derefMember) }" }
    elsif ($alias eq 'first_index')   { $str = "sub { return &List::MoreUtils::first_index($secondArgument, $derefMember) }" }
    elsif ($alias eq 'reduce')        { $str = "sub { return List::Util::reduce { $secondArgument->($a, $b) } $derefMember }" }
    elsif ($alias eq 'natatime')      { $str = "sub { my \$iter = List::MoreUtils::natatime($secondArgument, $derefMember);
                                                    if ($thirdArgument) {
                                                      while (my \@vals = \$iter->()) {
                                                        $thirdArgument->(\@vals)
                                                      }
                                                      return
                                                    } else {
                                                      return \$iter
                                                    }
                                                   }" }
    elsif ($alias eq 'shallow_clone') { $str = "sub { return [$derefMember] }" }
    elsif ($alias eq 'map')           { $str = "sub { return CORE::map { $secondArgument->(\$_) } $derefMember }" }
    elsif ($alias eq 'grep')          { $str = "sub { return CORE::grep { $secondArgument->(\$_) } $derefMember }" }
    elsif ($alias eq 'sort')          { $str = "sub { return $secondArgument ? CORE::sort { $secondArgument->(\$a, \$b) } $derefMember : CORE::sort $derefMember }" }
    elsif ($alias eq 'reverse')       { $str = "sub { return CORE::reverse $derefMember }" }
    elsif ($alias eq 'sort_in_place') { $str = "sub { return $derefMember = $secondArgument ? CORE::sort { $secondArgument->(\$a, \$b) } $derefMember : CORE::sort $derefMember }" }
    elsif ($alias eq 'splice')        { $str = "sub { return CORE::splice $derefMember, $secondArgument, $thirdArgument, $stackFromFourthArgument }" }
    elsif ($alias eq 'shuffle')       { $str = "sub { return List::Util::shuffle($derefMember) }" }
    elsif ($alias eq 'uniq')          { $str = "sub { return List::MoreUtils::uniq($derefMember) }" }
    elsif ($alias eq 'delete')        { $str = "sub { return CORE::splice($derefMember, $secondArgument, 1) }" }
    elsif ($alias eq 'insert')        { $str = "sub { return CORE::splice($derefMember, $secondArgument, 1, $thirdArgument) }" }
    elsif ($alias eq 'flatten_deep')  { $str = "sub { return $_flatten_deep($derefMember, $secondArgument) }" }
    elsif ($alias eq 'elements')      { $str = "sub { return $derefMember }" }
    elsif ($alias eq 'flatten')       { $str = "sub { return $derefMember }" }
    elsif ($alias eq 'join')          { $str = "sub { return CORE::join($secondArgument // ',', $derefMember) }" }
    elsif ($alias eq 'print')         { $str = "sub { print { $secondArgument || *STDOUT } CORE::join((defined($thirdArgument) ? $thirdArgument : ','), $derefMember) }" }
    _commit($myOpts, $name, $handles, $stubname, $alias, $str);
    #
    # Common operators
    #
    _handles_via_common($myOpts, $name, $handles, $stubname, $alias, $memberValue, $thirdArgument, $fourthArgument)
  }
}

sub _handles_via_Hash {
  my ($myOpts, $name, $handles) = @_;

  my $accessor    = "{$name}";
  my $member      = $firstArgument . '->' . $accessor;
  my $memberValue = $member . '->' . "{$secondArgument}";
  my $derefMember = "\%{$member}";

  my ($str, $coderef);
  while (my ($stubname, $alias) = each %{$handles}) {
    ($str, $coderef) = (undef, undef);
    if    ($alias eq 'delete')        { $str = "sub { return CORE::delete($memberValue) }" }
    elsif ($alias eq 'keys')          { $str = "sub { return CORE::keys($derefMember) }" }
    elsif ($alias eq 'exists')        { $str = "sub { return CORE::exists($memberValue) }" }
    elsif ($alias eq 'defined')       { $str = "sub { return CORE::defined($memberValue) }" }
    elsif ($alias eq 'values')        { $str = "sub { return CORE::values($derefMember) }" }
    elsif ($alias eq 'kv')            { $str = "sub { return $derefMember }" }
    elsif ($alias eq 'clear')         { $str = "sub { return $derefMember = () }" }
    elsif ($alias eq 'count')         { $str = "sub { return CORE::scalar(CORE::keys($derefMember)) }" }
    elsif ($alias eq 'is_empty')      { $str = "sub { return $derefMember ? 0 : 1 }" }
    elsif ($alias eq 'shallow_clone') { $str = "sub { return {$derefMember} }" }
    _commit($myOpts, $name, $handles, $stubname, $alias, $str);
    #
    # Common operators
    #
    _handles_via_common($myOpts, $name, $handles, $stubname, $alias, $memberValue, $thirdArgument, $fourthArgument)
  }
}

sub _handles_via_String {
  my ($myOpts, $name, $handles) = @_;

  my $accessor    = "{$name}";
  my $member      = $firstArgument . '->' . $accessor;
  my $memberValue = $member;

  my ($str, $coderef);
  while (my ($stubname, $alias) = each %{$handles}) {
    ($str, $coderef) = (undef, undef);
    if    ($alias eq 'prepend')     { $str = "sub { return $memberValue = $secondArgument . $memberValue }" }
    elsif ($alias eq 'replace')     { $str = "sub { return do {
                                                                        (&Scalar::Util::reftype($secondArgument) // '') eq 'CODE' ?
                                                                        $memberValue =~ s/$firstArgument/$secondArgument->()/e
                                                                        :
                                                                        $memberValue =~ s/$firstArgument/$secondArgument/
                                                                      }, $memberValue }" }
    elsif ($alias eq 'chop')        { $str = "sub { return CORE::chop($memberValue) }" }
    elsif ($alias eq 'chomp')       { $str = "sub { return CORE::chomp($memberValue) }" }
    elsif ($alias eq 'clear')       { $str = "sub { return $memberValue = '' }" }
    elsif ($alias eq 'length')      { $str = "sub { return CORE::length($memberValue) }" }
    elsif ($alias eq 'substr')      { $str = "sub { return ($nbArguments >= 4) ?
                                                                      CORE::substr($memberValue, $firstArgument, $secondArgument, $thirdArgument)
                                                                      :
                                                                      ($nbArguments == 2) ?
                                                                      CORE::substr($memberValue, $firstArgument, $secondArgument)
                                                                      :
                                                                      CORE::substr($memberValue, $firstArgument) }" }
    _commit($myOpts, $name, $handles, $stubname, $alias, $str);
    #
    # Common operators
    #
    _handles_via_common($myOpts, $name, $handles, $stubname, $alias, $memberValue, $secondArgument, $thirdArgument)
  }
}

sub _handles_via_Number {
  my ($myOpts, $name, $handles) = @_;

  my $accessor    = "{$name}";
  my $member      = $firstArgument . '->' . "$accessor";
  my $memberValue = $member;

  my ($str, $coderef);
  while (my ($stubname, $alias) = each %{$handles}) {
    ($str, $coderef) = (undef, undef);
    if    ($alias eq 'abs')         { $str = "sub { return CORE::abs($memberValue) }" }
    _commit($myOpts, $name, $handles, $stubname, $alias, $str);
    #
    # Common operators
    #
    _handles_via_common($myOpts, $name, $handles, $stubname, $alias, $memberValue, $secondArgument, $thirdArgument)
  }
}

sub _handles_via_Bool {
  my ($myOpts, $name, $handles) = @_;

  my $accessor    = "{$name}";
  my $member      = $firstArgument . '->' . "$accessor";
  my $memberValue = $member;

  my ($str, $coderef);
  while (my ($stubname, $alias) = each %{$handles}) {
    ($str, $coderef) = (undef, undef);
    if    ($alias eq '='     ) { $str = "sub { return $memberValue = 1 }" }
    elsif ($alias eq 'unset' ) { $str = "sub { return $memberValue = 0 }" }
    elsif ($alias eq 'toggle') { $str = "sub { return $memberValue = $memberValue ? 0 : 1 }" }
    _commit($myOpts, $name, $handles, $stubname, $alias, $str);
    #
    # Common operators
    #
    _handles_via_common($myOpts, $name, $handles, $stubname, $alias, $memberValue, $secondArgument, $thirdArgument, '=' => 1)
  }
}

sub _handles_via_Code {
  my ($myOpts, $name, $handles) = @_;

  my $accessor    = "{$name}";
  my $member      = $firstArgument . '->' . "$accessor";
  my $memberValue = $member;

  my ($str, $coderef);
  while (my ($stubname, $alias) = each %{$handles}) {
    ($str, $coderef) = (undef, undef);
    if ($alias eq 'execute' )        { $str = "sub { return $memberValue->($stackFromSecondArgument) }" }
    if ($alias eq 'execute_method' ) { $str = "sub { my \$method = $memberValue; return $secondArgument->\$method($stackFromThirdArgument) }" }
    _commit($myOpts, $name, $handles, $stubname, $alias, $str);
    #
    # Common operators
    #
    _handles_via_common($myOpts, $name, $handles, $stubname, $alias, $memberValue, $secondArgument, $thirdArgument)
  }
}

sub _handles_via_common {
  my ($myOpts, $name, $handles, $stubname, $alias, $memberValue, $realSecondArgument, $realThirdArgument, %exception) = @_;
  #
  # Not really ops, but we handle these common cases:
  # - Empty alias: this is the get
  # - Accessor
  #
  {
    my ($str, $coderef);
    if    (! $exception{''}         && $alias eq ''        ) { $str = "sub { return $memberValue }" }
    elsif (! $exception{'accessor'} && $alias eq 'accessor') { $str = "sub { return ($nbArguments == 2) ? $memberValue : $memberValue = $realThirdArgument }" }
    _commit($myOpts, $name, $handles, $stubname, $alias, $str)
  }
  #
  # As per perlop manual page
  # -------------------------
  #
  # Auto-increment and Auto-decrement
  #
  {
    my ($str, $coderef);
    if    (! $exception{'x++'} && $alias eq 'x++') { $str = "sub { return $memberValue++ }" }
    elsif (! $exception{'x--'} && $alias eq 'x--') { $str = "sub { return $memberValue-- }" }
    elsif (! $exception{'++x'} && $alias eq '++x') { $str = "sub { return ++$memberValue }" }
    elsif (! $exception{'--x'} && $alias eq '--x') { $str = "sub { return --$memberValue }" }
    _commit($myOpts, $name, $handles, $stubname, $alias, $str)
  }
  #
  # Operators with an lhs and an rhs
  #
  if (grep { ! $exception{$_} && $alias eq $_ }
      (
       #
       # Assignment operators
       #
       '=',
       '**=', '+=', '*=', '&=', '&.=', '<<=', '&&=',
       '-=', '/=', '|=', '|.=', '>>=', '||=',
       '.=', '%=', '^=', '^.=', '//=',
       'x=',
       #
       # Multiplicative operators
       #
       '*', '/', '%',
       #
       # Additive operators
       #
       '+', '-', '.',
       #
       # Shift operators
       #
       '<<', '>>',
       #
       # Relational operators
       #
       '<', '>', '<=', '>=', 'lt', 'gt', 'le', 'ge',
       #
       # Equality operators
       #
       '==', '!=', '<=>', 'eq', 'ne', 'cmp', '~~',
       #
       # Bitwise operators
       #
       '&', '|', '^',
       'xor',
       #
       # Logical operators
       #
       '&&', '||',
       'and', 'or',
       #
       # Range operator
       #
       '..',
       #
       # Comma operators
       #
       ',', '=>',
      )) {
    my ($str, $coderef);
    $str = "sub { return $memberValue $alias $realSecondArgument }";
    _commit($myOpts, $name, $handles, $stubname, $alias, $str)
  }
  #
  # Special operators
  #
  if (grep { ! $exception{$_} && $alias eq $_ } ('?:')) {
    my ($str, $coderef);
    $str = "sub { return $memberValue ? $realSecondArgument : $realThirdArgument }";
    _commit($myOpts, $name, $handles, $stubname, $alias, $str)
  }
  #
  # Unary operators
  #
  if (grep { ! $exception{$_} && $alias eq $_ } ('!', 'not', '-', '~', '+', '\\')) {
    my ($str, $coderef);
    $str = "sub { return $alias$memberValue }";
    _commit($myOpts, $name, $handles, $stubname, $alias, $str)
  }
  #
  # Binding operators
  #
  if (grep { ! $exception{$_} && $alias eq $_ } ('=~', '!~')) {
    my ($str, $coderef);
    $str = "sub { return $memberValue $alias /$realSecondArgument/ }";
    _commit($myOpts, $name, $handles, $stubname, $alias, $str)
  }

  return
}

sub _commit {
  my ($myOpts, $name, $handles, $stubname, $alias, $str) = @_;

  return unless defined($str);

  my $coderef = eval $str;

  if (defined($coderef)) {
    printf STDERR "Installing %s::%s, aliased to '%s' on member '%s', as %s\n", $myOpts->{target}, $stubname, $alias, $name, $str if ($myOpts->{trace});
    $myOpts->{install_tracked}->($myOpts->{target}, $stubname, $coderef);
    delete($handles->{$stubname})
  } else {
    warn sprintf("Failed to install %s::%s, aliased to %s on member %s, as %s: %s\n", $myOpts->{target}, $stubname, $alias, $name, $str, $@)
  }

  return
}

#
# Copied from Data::Perl::Role::Collection::Array 0.002009
#
sub _flatten_deep {
  my @array = @_;
  my $depth = CORE::pop @array;
  --$depth if (defined($depth));

  my @elements = CORE::map {
    (ref eq 'ARRAY')
      ? (defined($depth) && $depth == -1) ? $_ : _flatten_deep( @$_, $depth )
        : $_
      } @array;
}

1;
