# useful 'Macros' for Ptml files

sub TodaysDate
{
#   my ($sec,$min,$hr,$mday,$mon,$yr) = localtime;
   return join("-",(split(" ",''.localtime))[2,1,4]);
}

# this 'Macro' encodes its parameter value for cgi usage
sub E #($one_value)  encode one value into cgi format
{  my ($encoded) = @_;

   # spaces to +'s
   $encoded =~ tr/ /+/;    

   # everything non-alpha-numeric into a hex encoded format
   $encoded =~ s/([^a-z+A-Z0-9])/"%".unpack("H",$1).unpack("h",$1)/ge;

   return $encoded;
}

# this 'Macro' counts the number of times it is called and returns the 
# indicated value every Nth time, and the other value every other time.
# The position of the macro in the file being scanned is used to uniquely
# identify the different occurences of the macro in the source code.

sub ONCOUNT #(number,string to show every nth time, string for other times)
{  my ($n,$oncount, $offcount) = @_;

   $ONCOUNT{tell()}{pos()}++;
   
   if ( $ONCOUNT{tell()}{pos()} == $n )
   {   $ONCOUNT{tell()}{pos()} = 0;
       return $oncount;
   }else
   {   return $offcount;
   }  
}

# this 'Macro' returns one value or the other. The boolean expression will
# already have been evaluated when this procedure is invoked.

sub IF #(expression_result,OnTrueText,OnFalseText)
{  my ($YN,$ontrue, $onfalse) = @_;
   return $ontrue if $YN;
   return $onfalse;
}

# this 'Macro' returns the first non blank value
sub FIRSTOF #(string1,string2,etc..)
{
   foreach (@_) { return $_ if "$_" ne ""; }
   return "";
}

# the '1' here returns TRUE to the 'require'r of this module
1;

