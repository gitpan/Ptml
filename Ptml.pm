#!/usr/bin/perl

# Documentation follows at the __END__, and can be read with `perldoc Ptml'.

#   '$Id: Ptml.pm,v 1.6.1.2 1999/11/04 13:14:34 malcolm Exp malcolm $
#
#   Ptml.pm is a Perl module to merge/format/render a PTML template file.
#   Copyright (C) 1999 Malcolm Dew-Jones.
#
#   This program is free software; you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation; either version 2 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program; if not, write to the Free Software Foundation, Inc.,
#   675 Mass Ave, Cambridge, MA 02139, USA.
#
#   Malcolm Dew-Jones can be reached at 73312.2317@compuserve.com or
#   yf110@freenet.victoria.bc.ca, or via snail mail 604 Cornwall Street,
#   Victoria B.C. Canada V8V 4L1 ';


my $user_eval;
my $make_user_eval = sub
{  $user_eval;
   my $s=
   ($Text::Ptml::verbose||$Text::Ptml::debugging)?
   '$user_eval = sub 
   {   my $s =eval "'.($Text::Ptml::package&&
                       "package $Text::Ptml::package; ").
                    '$_[0]";
       print "${Text::Ptml::errtag}$_[0],$@\\n" if $@;
       $s;
   };'."\n"
   :
   '$user_eval = sub 
   {   eval "'.($Text::Ptml::package&&
                 "package $Text::Ptml::package; ").
             '$_[0]"
   };'."\n";
##print $s;
   eval $s;
   print "${Text::Ptml::errtag}$s$@\n" if 
       $@ && 
       ($Text::Ptml::verbose || $Text::Ptml::debugging);
};

my $embed;
my $make_embedder = sub 
{  $embed;
   my $s=
   '$embed = sub 
   {   '.($Text::Ptml::package&&
          "package $Text::Ptml::package;\n       ").
       '# pre filter
       '.$Text::Ptml::pre_filter.';
       # embedding
       '.($Text::Ptml::embedding&&'s/{\+\s*(.*?)\+}/$1/eeg').';
       '.( ( ($Text::Ptml::verbose 
              || $Text::Ptml::debugging) 
            && $Text::Ptml::embedding)
          &&'print( "${Text::Ptml::errtag}$1:$@\n",undef $@) if $@;').'
       # post filter 
       '.$Text::Ptml::post_filter.';
       # printing
       '.($Text::Ptml::printing&&'print').';
   };'."\n";
   eval $s;
   print "${Text::Ptml::errtag}$s$@\n" if 
       $@ && $Text::Ptml::verbose 
       || $Text::Ptml::debugging;
};

my $user_loop = sub #(clause, loop function ref )
{  my $s= ($Text::Ptml::package&&
           "package $Text::Ptml::package;\n").
          "$_[0] { &{\$_[1]}};\n";
   print "${Text::Ptml::errtag}$s" if $Text::Ptml::debugging;
   eval $s;
   print "${Text::Ptml::errtag}$@\n" if 
       $@ && 
       ($Text::Ptml::verbose || $Text::Ptml::debugging);
};

my $user_call = sub #(function ref, params_str )
{  my $s= ($Text::Ptml::package&&
           "package $Text::Ptml::package;\n").
          "&{\$_[0]}($_[1]);\n";
   print "${Text::Ptml::errtag}$s" if $Text::Ptml::debugging;
   eval $s;
   print "${Text::Ptml::errtag}$@\n" if 
       $@ && 
       ($Text::Ptml::verbose || $Text::Ptml::debugging);
};

package Text::Ptml;

$VERSION=(q{$Revision: 1.6.1.2 $}=~m/(\d+\.\d+)/)[0];
                                                   
$embedding   =1                                 unless defined $embedding   ;
$printing    =1                                 unless defined $printing    ;
$debugging   =$ENV{Ptml_DEBUGGING}              unless defined $debugging   ;
$set_verify  =$ENV{Ptml_SET_VERIFY}             unless defined $set_verify  ;
$verbose     =$ENV{Ptml_VERBOSE} || $set_verify unless defined $verbose     ;
$pre_filter  =''                                unless defined $pre_filter  ;
$post_filter =''                                unless defined $post_filter ;
$package     =''                                unless defined $package     ;
$comments    ='#'                               unless defined $comments    ;
$cmdtag      ='(?:<BR>){0,1}\s*\.'              unless defined $cmdtag      ;
$vertag      ='<BR>::'                          unless defined $vertag      ;
$errtag      ='<BR>::'                          unless defined $errtag      ;

&$make_user_eval();
&$make_embedder();

my $Module;
($Module=(caller(0))[6])=~s/[:\/\\]/::/g;
$Module=~s/\.[pP][mM]$//;

my $showing             =1;
my $defining_subroutine =0;
my $subroutines         ={};
my $sections            ={};
my @IFs                 =();
my $skip_loop           =0;
my $loop_depth          =0;
my $subroutine_depth    =0;
my $FileSym             ='Text::Ptml::TEMPLATE';
my @FileSyms            =();

$ptml_eval = sub                                                       #X#
{                                                                      #X#
   eval $_[0];                                                         #X#
   print "${errtag}$@\n" if $@ && ($verbose || $debugging);            #X#
};                                                                     #X#

sub scanthruPtml #(\*INFILE,position)
{  
   my ($INFILE) = shift;
   my ($position)= shift || 0;
   seek $INFILE , $position , 0;

   print "${vertag}scanthruPtml at $position\n" if $set_verify;

   while (<$INFILE>)
   {   
       print "${vertag}",$_  if $set_verify;

       if (not m/^${cmdtag}/io) 
       {   # we get here if it cant possibly be a command
           # various conditions make us skip over the text

           next if (not $showing) 
                or ($defining_subroutine > 0) 
                or ($skip_loop > 0) 
                or (@IFs>0 and $IFs[$#IFs] <= 0);

           &$embed;
           next;
       }
       # we can only get here if it might be a command
       
       next if m/^${cmdtag}$comments/io ;    # skip comments

       if (m/^${cmdtag}SECTION\s+(\S.*)/io)
       {   # start of section, is it one we want?
           my (@names) = split(' ',$1);
           print "${errtag} sections ",join(',',@names),"\n"  
               if $debugging;
           foreach (@names)
           {   last if $showing = $sections->{uc $_}||$sections->{'*'};
           }
           next;
       }elsif
           (m/^${cmdtag}END_SECTION/io)
       {   # end of section, display rest of file by default
           $showing = 1;
           next;
       }
       next if not $showing;
       
       if (m/^${cmdtag}SUBROUTINE\s+(\S+)\s*(.*)/io)
       {   $defining_subroutine++;
           my ($subroutine_name) = $1;
           my ($subroutine_params) = $2 || '@argv';
           my ($subroutine_tell) = tell $INFILE;

           my $s=
           'my $s=sub 
           {   if ($Text::Ptml::debugging) 
               {   for (my $i=0 ; $i<@_ ; $i++) 
                   {   print "${$Text::Ptml::errtag}\\@[$i]=$_[$i]\\n";
                   }
               }'."
               print '${Text::Ptml::vertag}local ($subroutine_params) = \@_;'." . '"\\n"
                   if $Text::Ptml::set_verify;
               '.($Text::Ptml::package&&
              "package $Text::Ptml::package; ")."
               local ($subroutine_params) = \@_;
               Text::Ptml::scanthruPtml(\\".*{$INFILE}.",$subroutine_tell);
           }\n";
           print ${errtag},$s if $debugging;
           $subroutines{$subroutine_name} = &$user_eval($s);
       }elsif
          ($defining_subroutine >0 
           and m/^${cmdtag}END_SUBROUTINE/io)
       {   $defining_subroutine --;
           next;
       }

       next if $defining_subroutine > 0;

       if (@IFs >0)                    # if we're in an IF (may fall thru)
       {   if (m/^${cmdtag}ELSE/io)
           {   $IFs[$#IFs] = -1 * $IFs[$#IFs]; # toggle showing/notshowing
               next;
           }elsif
               (m/^${cmdtag}ELSIF\s+(\S.*)/io)
           {   next if $IFs[$#IFs] == 0;       # not showing 
               next if --$IFs[$#IFs] == 0;     # no longer showing 
               $IFs[$#IFs] = &$user_eval($1)?1:-1; # true=positive, false=negative
               next;
           }elsif
               (m/^${cmdtag}END_IF/io)
           {   pop @IFs;
               next;
           }elsif
               ($IFs[$#IFs] <= 0)              # in IF, are we IF-not-showing?
           {   # recognize IF levels even if not displaying
               push @IFs , 0 if (m/^${cmdtag}IF\s+\S.*/io);
               next;
           }
           # ELSE FALL THRU
       }

       if (m/^${cmdtag}IF\s+(\S.*)/io)
       {   # IF (expression)
           push @IFs , &$user_eval($1)?1:-1; # true=positive, false=negative
       }elsif
           (m/^${cmdtag}END_LOOP/io )
       {   last if $loop_depth>0;
           print $errtag,$_,"...but not in loop.\n" if ($verbose || $debugging);
       }elsif
           (m/^${cmdtag}(FOREACH|WHILE|FOR)\s+(\S.*)/io)
       {   my ($forwhile_clause) = $2;
           my ($forwhile) = lc $1;

           $loop_depth++;

           my $at_least_once = 0;
           my $told = tell $INFILE;

           if ($skip_loop <=0)
           {   my (@saveIFs) = @IFs;
               my $scanthruloop = sub
               {   $at_least_once = 1;
                   @IFs = @saveIFs;  # restore state of IFs each time

                   Text::Ptml::scanthruPtml($INFILE,$told);
               };
               &$user_loop("$forwhile $forwhile_clause",$scanthruloop );
           }
           if (not $at_least_once)
           {   $skip_loop ++;
               scanthruPtml($INFILE, $told);
               $skip_loop --;
           }

           $loop_depth--;

       }elsif
           (m/^${cmdtag}INCLUDE\s+(\S.*)/io)
       {   
           my (@saveIFs) = @IFs;
           my ($told) = tell $INFILE;
           $FileSym .= '_';
           push @FileSyms,$FileSym ;
           my $s=
           qq{open( $FileSym , "<$1" ) or die \$!;\n}.
           qq{Text::Ptml::scanthruPtml(\\*$FileSym);\n};
           # no close in case of defined SUBS
           &$user_eval($s);
           seek $INFILE , $told , 0;
           @IFs    = @saveIFs;
       }elsif
           (m/^${cmdtag}(?:END_SUBROUTINE|EXIT_SUBROUTINE)/io)
       {   last if $subroutine_depth > 0;
           print $errtag,$_,"...but not in subroutine.\n" if ($verbose || $debugging);
       }elsif
           (m/^${cmdtag}EVAL\s+(.*)/io)
       {   &$user_eval( $1);
       }elsif
           (m/^${cmdtag}PTML\s+(.*)/io)
       {
           &$ptml_eval( $1);
           &$make_user_eval;
           &$make_embedder;
       }elsif
           (m/^${cmdtag}CALL\s+(\S+)(.*)/io)
       {   
           my (@saveIFs) = @IFs;
           my ($told) = tell $INFILE;
           $subroutine_depth++;
           &$user_call($subroutines{$1}, $2 );
           $subroutine_depth--;
           seek $INFILE , $told , 0;
           @IFs    = @saveIFs;
       }else
       {   
           next if ($skip_loop > 0);   # inside a non-showing loop
           &$embed;
       }
   }   # end while
}

my $_import = sub 
{
   shift;
   my ($callpkg) = caller;
   @_ = qw{ PtmlPrint PtmlPrintMe PtmlMerge } if @_ == 0;
   eval "*${callpkg}::$_=*Text::Ptml::$_" while $_ = shift @_;
};

*Text::Ptml::import = $_import;
*{"${Module}::import"} = $_import;

sub ON  {1}
sub OFF {''}

sub PUSH
{  my($sym,$val)=@_;
   $sym = lc $sym; # all our variables are lowcase names
   my $s="\npush \@$sym , \$$sym\n";
   print $errtag,$s if $debugging;
   eval $s;
print "${errtag}PUSH 1: $@\n" if $@;
   print "${errtag}\$$sym = \$val\n" if $debugging && defined $val;
   eval "\$$sym = \$val" if defined $val;
print "${errtag}PUSH 2: $@\n" if $@;
}

sub POP
{  my($sym)=@_;
   $sym = lc $sym; # all our variables are lowcase names
   my $s = "\$$sym=pop(\@$sym);";
   eval $s;
   print "\n${errtag}$s\n" if $debugging;
print "${errtag}POP $@\n" if $@;
}

sub REMOVE
{  my($sym,$val)=@_;
   $sym = lc $sym; # all our variables are lowcase names
##print "\$$sym =~ s/\\Q\$val//;";
   eval "\$$sym =~ s/\\Q\$val//;";
print "${errtag}REMOVE: $@\n" if $@;
}

# backwards compatible sort of
$JOINSOME = q{s/\\\\\n\\Z//o;};
$JOIN     = 'chomp;';
$TRIM     = q{s/^[ \\t]+\\|?//o;};

my $make_sections = sub
{  $sections={};
   foreach (@{$_[0]}) { $sections->{uc $_}=1; }
};

my $make_options = sub                                                 #X#
{  # $_[0] = ref to hash                                               #X#
   my ($S,$s);                                                         #X#
   foreach (keys %{$_[0]})                                             #X#
   {                                                                   #X#
       $s="\$$_=$_[0]->{$_}\n";                                        #X#
       eval $s;                                                        #X#
       die "Bad Ptml Option: $s,$@\n" if $@;                           #X#
   }                                                                   #X#
};                                                                     #X#

sub PtmlMerge
{  my $INFILE = shift;
   my $OUTFILE = shift;
   my $section_list = shift || [];
   my $option_list = shift || {};

   &$make_sections($section_list);
   &$make_options($option_list);
   &$make_user_eval();
   &$make_embedder();

   my $ofh = select($OUTFILE);
   scanthruPtml($INFILE,tell $INFILE);
   select($ofh);
   close $_ while $_ = shift @FileSyms;
}

sub PtmlPrint
{  my ($filename)   = shift;
   my $section_list = shift || [];
   my $option_list = shift || {};

   &$make_sections($section_list);
   &$make_options($option_list);
   &$make_user_eval();
   &$make_embedder();

   push @FileSyms,$FileSym ;
   open( $FileSym , "<$filename" ) or die $!;
   scanthruPtml(\*{$FileSym});
   close $_ while $_ = shift @FileSyms;
}

sub PtmlPrintMe
{
   my $section_list = shift || [];
   my $option_list = shift || {};

   &$make_sections($section_list);
   &$make_options($option_list);
   &$make_user_eval();
   &$make_embedder();

   scanthruPtml(\*{"${callpkg}::DATA"}, tell "${callpkg}::DATA");
   close $_ while $_ = shift @FileSyms;
}


# the '1' here returns TRUE to the 'require'r of this module
1;

__END__

This file Copyright (C) 1999 Malcolm Dew-Jones.

=head1 NAME

Ptml - Procedural Template Merge Language

=head1 SYNOPSIS

    Loading the module

C<use Text::Ptml;     OR>

C<use Ptml;>  (Depending on where it is installed.)

    Merging data

C<PtmlPrint("filename", \@sections, \%options);>

C<PtmlPrintMe(\@sections, \%options);>

C<PtmlMerge(\*TEMPLATE, \*OUTPUT, \@sections, \%options);>


=head1 DESCRIPTION

PTML is a "language" used to control the merging of Perl data into template
files.  PTML statements are included as part of the text of a template.

I<Ptml.pm> is the module that contains the procedures used to perform the merge.

A template is some text which is stored in one or more files, or embedded in
the __DATA__ segment of your Perl program.  The template must be stored in a
file, as opposed, perhaps, to a Perl variable.  (Actually a pipe may also be 
used but with reduced merge functionality - commands requiring 
backtracting, such as loops, will not work.)

The template includes text that is displayed verbatim, macros that are
replaced by values during the merge, and PTML language statements that
control the merge process.
Various options can be used to modify or enhance the merge process.

PTML language statements look vaguely similar to RUNOFF commands, and macros
are snippets of Perl code bracketed on a single line by C<{+> and C<+}>.

The merge is tightly integrated with your Perl code - it accesses all your
Perl elements, data, subs, file handles, etc., exactly as if it were defined
as a regular subroutine within your program.  

Data for the merge is not explicitly passed to the template, but instead
consists of what ever Perl variables (or subroutine results) the template
decides to use.  It is the responsibility of the calling Perl code to have
prepared what ever data the template requires. (Though missing data will not
normally cause any problems.)

It is convenient to provide well structured data via one or more aptly named
hash arrays, but this is not required.

By default the merge procedure runs within the package of the I<first>
module that I<use>'s Ptml.  If Ptml is C<use>'d near the top of a typical
Perl program then the merge takes place in the C<main::> package.  The
I<package> option can over ride this.

=head1 SIMPLE EXAMPLE

The following text is a short, introductory example to what a simple
template file might look like, if I<cat>'d or I<TYPE>'d to the screen.

-----------------------------------------------------------
Z<>I<.SECTION NEW_USER>

Welcome to our BBS system.  There are
I<{+$number_of_accounts+}> other users that have accounts
here.  If you would like to join then select the NEW USER
option when it appears below.

Z<>I<.SECTION RETURNING_USER>

Welcome back, I<{+$the_user+}>, to our BBS system.

Z<>I<.END_SECTION>

Right now there are I<{+$number_of_users+}> other users 
logged on, and you can chat with them if you wish.

 They are...

Z<>I<.FOREACH $user (@current_users)>

I<{+$user+}>.  I<{+he_she_of($user)+}> likes I<{+likes_of($user)+}>, 
but hates I<{+dislikes_of($user)+}>.

Z<>I<.END_LOOP>

 Press Enter to continue...
-----------------------------------------------------------

The above template might be displayed by code such as the following...

   #!/perl
   use BBS_utils;
   use Text::Ptml;
   
   @current_users=split(/\s+.*\n/, `who` );
   $the_user=$ENV{LOGNAME};
   
   if ($the_user eq 'guest')
   {   PtmlPrint("BBS_menu.ptml",[NEW_USER]);
   }else
   {   PtmlPrint("BBS_menu.ptml",[RETURNING_USER]);
   }


=over 5

=head1 MERGE FUNCTIONS

=item PtmlPrint( "Filename" [,\@section_names [,\%options]] )

This function reads the template from a file, and prints the output to the
currently I<select>'ed file handle (STDOUT by default).

I<Filename> is the name of the file containing the template.

I<section_names> is optional, and is a reference to an array of strings,
each of which is the name of a section to be shown. (Sections, as discussed
below, are indicated with the .SECTION statement.)

If no section names are specified then no named section of the template will
be merged.  As a special case, the section name I<*> will select all
sections.

I<options> is optional, and is a reference to a hash.  Each key is an option
name, and its value is the desired starting value for that option.  Options
are discussed under the 'OPTIONS' heading.  Some options can only be
specified for the first merge, and remain fixed for any subsequent merges
unless the Ptml.pm module is reloaded via the Perl I<do> statement.


=item PtmlPrintMe( [ \@SECTION_NAMES ] )

This function reads the template from the __DATA__ section of a Perl
program, and prints the output to the
currently I<select>'ed file handle (STDOUT by default).

To be specific, its reads the DATA handle in the package of
the code that invokes PtmlPrintMe.

I<section_names> and I<options> are discussed above.

=item PtmlMerge( \*TEMPLATE , \*OUTPUT [,\@SECTION_NAMES ] )

This function reads the template from a file handle, and prints the result
to the specified file handle.

I<TEMPLATE> is the file handle of an open file (or pipe) that contains the
template.

I<OUTPUT> is the handle to which the data is `print'ed.  It can be any type of
handle that can be used by `print', E.g. a pipe to I<sendmail>.

I<section_names> and I<options> are discussed above.

=back

=head1 PTML LANGUAGE STATEMENTS

=over 5

=item .SECTION name [name ...]

Indicates the start of a named section of the template.  A section can have
more than one name, in which case it will be formatted whenever any of the
indicated sections are being merged.  A name can be assigned to more than
one section.  In this manner a single section name can be used to select
more than one section.  (The merge functions have additional ways of
selecting multiple sections.)

Portions of the template that lie outside of any .SECTION or .END_SECTION
are always formatted.  Each new .SECTION ends any previous .SECTION.


=item .END_SECTION

Indicates the end of any previous .SECTION.

=item .SUBROUTINE name [arg1 , arg2 , ...]

Marks the beginning of a template subroutine.  The template text upto the
corresponding .END_SUBROUTINE is not printed when first encountered, but
instead is saved for later and formatted when a I<.CALL name> statement is
encountered with this subroutine name.  The arguments are made available as
perl variables for use as Ptml macros within the text of the subroutine.

If no args are specified in the subroutine definition then an array called
I<@args> is created to hold any parametres passed when the subroutine is
invoked.

Subroutine definitions may be nested, though there is no particular use
for this fact in this version of Ptml.

=item .EXIT_SUBROUTINE

Any currently formatting subroutine is exited.

=item .END_SUBROUTINE

This statement marks the end of the defintion of a subroutine.

=item .CALL name parametre_expr

The merge process I<calls> the indicated subroutine.  This actually means
that the merge temporarily takes place starting at the text immediately
following the corresponding .SUBROUTINE statement.  The I<parametre_expr>
becomes the list of arguments in a Perl function call, so its syntax is
anything that Perl accepts.  The debugging option will display the generated
perl code which may be useful when first using parametres in subroutines.

=item .IF expr

=item .ELSIF expr

=item .ELSE

=item .END_IF

Standard .IF logic.  I<expr> is passed directly to Perl so its syntax is
anything that Perl accepts.  The .IF nesting is independent of the loop
nesting, so that statements such as the following are possible...

   .IF $usingDynamicList
       .WHILE ($next=GetNext())
   .ELSE
       .FOREACH $next (@staticList)
   .END_IF
           the next item is {+$next+}
       .END_LOOP

though I'm not suggesting you will want to use this capability very often.


=item .EVAL expr

Passes I<expr> to the Perl I<eval> command.  It is the literal text of
I<expr> that is eval'd.  Variables embedded within the expression are not
expanded unless the expression includes code to force that expansion.

=item .PTML expr

A Ptml specific eval used to set PTML options.  The .PTML statement
evaluates I<expr> in the Ptml package space, and also forces various
internal items to be recompiled.

Various items are provided for use in the .PTML statement.  They assist in
setting the various options. (PTML options are listed and discussed in
another section.)

=over 8

=item .PTML PUSH(option_name [,new_value] )

=item .PTML POP(option_name)

The current value of any option can be saved and restored using I<PUSH> and
I<POP>.  These functions accept the name of the option to save or restore.

   E.g.    .PTML PUSH(pre_filter)    #Note: no $

When an option is pushed then it retains its current value, unless a new
value is also specified.  (The new value may be modified later.)

=item .PTML REMOVE(options_name,string)

This may assist in manipulating the filters.  It is a wrapper around
I<s/your-string//;>, but is less typo prone.  

   E.g.    .PTML $pre_filter.=$TRIM
           .PTML REMOVE(pre_filter,$TRIM)

=item predefined filter: $TRIM

=item predefined filter: $JOIN

=item predefined filter: $JOINSOME

Previous version of Ptml provided an I<.EDIT_LINES> statement.  It accepted
several options to modify the input lines of the template.  These three
variables, which are accessible within the .PTML statement, reproduce that
functionality when used with the I<pre_filter> option.

   E.g.    .PTML $pre_filter=$TRIM

I<$TRIM> removes any white space from the beginning of each line, plus if
the line begins with I<|> then it removes that character as well. 

I<$JOIN> removes the trailing I<\n> from each template line.
This `joins' the output lines into a
single line.

Note: You may wish to include one extra blank line I<after> the
joined lines to force a new line before further output.

   .PTML $pre_filter=$JOIN
   These lines 
   Will be joined together.
   .PTML undef $pre_filter

   Note: the previous blank line forces a new line

I<$JOINSOME> removes the trailing I<\n> from lines that end in 
a I<\>.  The slash is removed as well as the <\n>.

   .PTML $pre_filter=$JOINSOME
   These lines \
   Will be joined together.
   Ane so will \
   These two lines.
   .PTML undef $pre_filter

=back

=over 5

=item .INCLUDE filename

Merges the indicated file.  The filename can include Perl variables, so 
statements such as the following are possible...

   E.g.    .INCLUDE $TemplateDir/Header.html

(where $TemplateDir would be a Perl variable containing a directory name).

=item .FOREACH expr

=item .FOR expr

=item .WHILE expr

=item .END_LOOP 

Each loop keyword is translated directly into its obvious Perl equivalent.
I<expr> is passed directly to Perl so its syntax is anything that Perl
accepts.  The loop continues until the corresponding .END_LOOP is encountered. 

   e.g. .FOREACH $i (@SomeValues)
           ... text to repeatedly display goes here ...
        .END_LOOP 

   e.g. .FOR ($i=0;$i<4; $i++)
           ... text to repeatedly display goes here ...
        .END_LOOP 

   e.g. .WHILE ($i<4)
           ... text to repeatedly display goes here ...
        .END_LOOP 

Due to the independance of IF logic and the LOOP logic there is a semantic
bug.  The bug does not alter the text displayed by the merge, but care must
be taken if an IF statement inside a loop has side effects and depends on
the loop's logic.  Examples and solutions are shown in the BUGS section.

=back

=head1 PTML OPTIONS

A variety of options are available to control aspects of the merge.  Each
option corresponds to a simple Perl variable in the Text::Ptml
package.  Most options can be modified at any time, but a few, as noted
below, can only be set before any data is merged - once any data has been
merged then these options can not be modified unless the I<Ptml.pm> file is
reloaded using the Perl I<do> statement.

Options can be set as Perl variables.

   E.g.  $Text::Ptml::debugging=1;

Options can be set by name in the I<\%options> parametre of each merge
function.  In this case the values I<ON> and I<OFF> can be used for
readability, as well as normal values (e.g. I<1>, I<0>, I<"JOIN">, etc).

   E.g.  PtmlPrintMe(\@sections, {debugging=>ON} );

Options can be set as perl variables within the .PTML language statement.  The
values ON and OFF may be used.  The package name does not need to be included.

   E.g.  .PTML $debugging=ON

An option may be the target of the I<PUSH>, I<POP>, and I<REMOVE> functions,
which are available from within the .PTML language statement.  In this case
the option's name only (not its value) is used - i.e. leave of the I<$>.

   E.g.  .PTML PUSH(debugging)

Three debugging options, I<debugging>, I<verbose>, and I<set_verify>, can
also be set from the command line by assigning an environment variable
before running the perl script.

   E.g.  bash shell

         $ Ptml_DEBUGGING=1
         $ export Ptml_DEBUGGING

   E.g.  DOS

         C:> SET Ptml_DEBUGGING=1


Note: each option example shown below illustrates just one of the several
possible techniques for assigning values to the option.  Different
techniques are shown for completeness, not because the technique shown is
specific to that option.


=over 5

=item embedding

Controls whether macros are expanded or not.  Long portions of a template
that do not contain macros will merge somewhat faster if embedding is turned
off, and this is one way to display macro text as-is.

   E.g.    .PTML $embedding=ON

=item printing

Controls whether the merged template is printed.  This can be useful for
merging a template in several passes (which is useful if a template contains
embedded data that is used before it is defined) or to view certain
debugging output.

   E.g.    PtmlPrintMe(\@sections, {printing=>OFF} );

=item debugging

Various low level details of the merge can be displayed.  This is most
useful to ensure that any Perl code specified by the template is being
interpreted correctly.

   E.g.    $Text::Ptml::debugging=1;

=item set_verify

The source of the template can be displayed as it is being merged.

   E.g.    $ Ptml_SET_VERIFY=1
           $ export Ptml_SET_VERIFY
           $ perl my_merge_script

=item verbose

Perl eval errors are reported.

   E.g.    .PTML $verbose=1;

=item pre_filter

The pre_filter is Perl code that runs just before macros are embedded.  It
can be any perl code you wish, though a simple subroutine name is the
easiest to specify.  The pre_filter is defined within the Ptml package
space, but runs in the callers package space.  Several Ptml variables are
provided that assist in backwards compatibility with earlier versions of
Ptml.

 E.g. user defined sub as a prefilter - remove certain lines
   
   # in caller's package
   sub NO_SECRETS { $_ = "*secret line removed*\n" if m/secret/o;}

   .# in the template
   .PTML pre_filter=NO_SECRETS 
 OR
   .PTML pre_filter='NO_SECRETS'

 E.g. hard coded filter - double spacing
   
   .PTML pre_filter='$_.="\\n"'

 E.g. using a predefined Ptml filter
   
   .PTML pre_filter=$TRIM

=item post_filter

The post_filter is set in the same manner as the pre_filter, but it runs
after any macros are embedded but before each line is printed.

 E.g.    

   sub count_characters {$count+=length($_)}
   $count =0;
   PtmlPrint("MyFile",[],{post_filter=>'count_chacters'});
   print "$count characters were output";
           
=item package

The package in which macros, expressions, and .EVAL's are evaluated.

By default the package used is the package current at the time of the
initial load of Ptml.pm.

   E.g.    PtmlPrintMe(\@sections,{package=>other_pkg});

=item comments

The comments option can only be set before any data is merged.  

The default comment statement is the single character I<#>, but
this can be replaced with any regular expression you wish.  If you match any
PTML language statements then you disable that statement.  Commented lines
are not seen by the filters, and are not output during the merge.

   E.g.    $Text::Ptml::comments='REMARK|COMMENT|#';

=item cmdtag

The cmdtag option can only be set before any data is merged.  It is the
regular expression that introduces every PTML command on a line.

The default command tag skips over a single optional HTML break tag <BR>
followed by any amount of white space and a trailing dot.

The cmdtag can be replaced with any regular expression you wish.

   E.g.    $Text::Ptml::cmdtag='\\\s*';

=item vertag

vertag is a string that is displayed at the start of each line echoed
because set_verify is turned on.

=item errtag

errtag is a string that is displayed at the start of each line echoed
as an error message because verbose or debugging is turned on.

=back

=head1 PTML MACROS

The text of a template can contain snippets of text that should be replaced
when the template is merged.  These are called I<macros>.

Each macro is bracketed by I<{+> and I<+}> .  This bracketing was chosen
because it will rarely clash with any text or macro contents.  The entire
macro must fit on a single line of the template.  

The contents of each macro is actually a snippet of Perl code that should
return the value to be embedded.  The simplest macro is a scalar string
variable.  However any amount of Perl code that will fit on a single line
can be used.

I find it useful to use Perl subs as macros to perform common formatting
tasks.  The file F<PtmlMisc.pl> contains various subs which I have found
useful on a variety of projects.  They are useful in their own
right, and also illustrate certain useful techniques.

The following 3 lines contain some simple examples of macros...

   1. The Perl variable $something has the value {+$something+}
   
   2. The current time as returned by localtime() is {+localtime()+}

   3. More complex Perl code can be used if desired... such as the 
      following code : {+$x==5?"X was 5":"X was not five"+}


=back

=head1 INSTALLATION

Ptml.pm can be installed in any directory by simply copying it to that
directory.  The path specified in the C<use> statement must then refer to
the install directory.

   E.g. if Ptml.pm is installed in the same directory as your script

      use Ptml;
   
   E.g. if Ptml.pm is installed in a Utils directory

      use Utils::Ptml;

   E.g. if Ptml.pm is installed in the Text/Template directory of 
        the Perl installation
        
      use Text::Ptml;

However, no matter where Ptml.pm is installed, it always uses the
Text::Ptml name space.  

By default, all three merge subroutines are exported into your name space.
Alternatively, you can list which subs should be exported, as discussed in
the standard Perl documentation, except that the numerous capabilities of
the standard Exporter module are not available.  Ptml.pm allows you
just to list the names of the subs to be imported.

   E.g. use Ptml qw{PtmlPrint};

   E.g. to prevent importing any of the Ptml routines
      
      use Ptml ();



=head1 SECURITY

The PTML file is as great a security risk as the Perl code which merges it.
While I do not believe that user data being merged I<into> a template poses an
I<inherent> security risk, it is certainly possible to write templates where
that would be the case.  (Examples shown below.)

ONE RULE IS ABSOLUTE - do not allow any untrusted data to be used as the
source of the template.

Data is merged using evals, but the eval's actually act upon the literal
text of the Perl variable names that your program uses as `macros', not on
the data contained within the macros.

The following template snippet shows some dos and don'ts.  You can run this
snippet using I<doPtml> if you wish by extracting the lines into a file.

=over 5 

=item -- Template snippet start --

Let's define two variables with potentially dangerous contents, just as if a
user was trying to weasel some nastiness into your template.  (These first
`.EVAL's are simply to make this self contained example work. In "real life"
the contents of the variables would come from a user, perhaps as an entry in
an HTML form.)

 .EVAL $ls_command1='`ls -l >> ls_output1`'
 .EVAL $ls_command2='`ls -l >> ls_output2`'

Note the evaluation quotes embedded in the strings.  Also note that
neither of the above .EVALS cause the commands to run because the string
uses ' quotes.

Now lets embed the value of the string in the template output.

$ls_command1 is the string {+$ls_command1+}. After this template is
merged you should look for the file `ls_output1'.  However you won't find
it. Embedding the string in this template does not cause the command to
run, even though the string contained `eval' quotes.

I wouldn't normally want to .EVAL any user input, but lets show that its
not inherently unsafe to do so!
 
 .EVAL $ls_command1
 
The ls command still didn't run.  The PTML EVAL is still acting on the
variable itself, not on its contents!

WHEREAS the following two template lines ARE DANGEROUS.  Note the
EXPLICIT DOUBLE EVALS.

     This embedding is dangerous, ouch! {+eval $ls_command2+}
     The following .EVAL is dangerous
     .EVAL eval $ls_command2

Afterwards you will find a file has been created - ls_output2, which
indicates that the second I<ls> comand, used in the double evals, ran.

=item -- Template snippet end --

=back

Counter examples are surely welcome.



=head1 BUGS

IF statement expressions contained within a loop being merged are always
evaluated even if the loop iterates zero times.  The documentation
below shows one possible work around.  (There are undoubtedly others.) If
an IF expression has no side effects then this issue can normally be
ignored as the result of the expression does not change the logic flow of
the template merging.

1. The following shows a benign occurence of the bug.

   e.g. .FOR ($i=1;$i<=$somevalue; $i++)
           .IF $i==1       (*) this is "always" evaluated at least once
               Start of list
           .END_IF
        .END_LOOP 

2. The following shows a dangerous occurence of the bug.  

   e.g. .FOR ($i=1;$i<=$number_of_files; $i++)
           .IF remove_file("File_Number_".$i)==$Err    (*) side effect
               Error removing File_Number_{+$i+}
           .END_IF
        .END_LOOP 


The line at (*) is evaluated at least once if that part of the
template is currently being merged.

If `$somevalue' is 0 then the loop iterates zero times and the text in the
loop is not displayed - *BUT* the line at (*) is evaluated once anyways. In
example 1 this makes no difference. However in example 2 the remove_file()
function would be called once to delete a file called "File_Number_".

The following example shows one way to protect against this bug if it's an
issue.

   e.g. #2
        .IF $somevalue>0
        .FOR ($i=0;$i<$somevalue; $i++)
           .IF $i==1       (*) this is NOT evaluated when $somevalue<=0
               Here we go
           .END_IF
        .END_LOOP 
        .END_IF

The outer IF/END_IF turns off the merge when the loop is going to iterate
zero times.  This prevents the IF at (*) from being evaluated.


=head1 AUTHOR

(C) Copyright 1999 Malcolm Dew-Jones.

Malcolm Dew-Jones, E<lt>F<73312.2317@compuserve.com>E<gt>.

=head1 LICENSE

Ptml is distributed under the terms of the GNU general license.

=head1 FILES

 Ptml.pm       module to be `require'd, and the pod documentation
 README        introduction and more documentation
 example.ptml  full examples of each statement
 example.data  used by examples.ptml
 doPtml        working example of routine to merge templates
 PtmlMisc.pl   useful `macro' functions
 
