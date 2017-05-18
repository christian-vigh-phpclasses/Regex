<?php
// test Regex class methods
// this script should be run in command-line mode
include ( 'Regex.class.php' ) ;

if  ( php_sapi_name ( )  ==  'cli' )
    {
	$eol	=  PHP_EOL ;
	$tab	=  "\t" ;
     }
else
    {
	$eol	=  "<br/>" ;
	$tab	=  str_repeat ( "&nbsp;", 8 ) ;
     }


// IsRegex() method example
$re	=  
   [
	'/foo bar/',				// valid
	'#foo bar#imsx',			// valid
	'/foo bar'				// invalid
    ] ;

foreach  ( $re  as  $item )
	echo ( "Regex::IsRegex ( $item ) = " . ( ( Regex::IsRegex ( $item ) ) ? 'true' : 'false' ) . $eol ) ;

echo ( $eol ) ;

// Matches() example 
$matches	=
   [
	[ 'file.txt', '*.txt' ],		// match
	[ 'file.txt', 'file.*' ],		// match
	[ 'dir/file.txt', '*.txt' ],		// no match
	[ 'dir/file.txt', 'dir/*.txt' ],	// match
	[ 'dir/file.txt', '*/*.txt' ],		// match
	[ 'dir/file.txt', '*\\file.txt' ]	// match
    ] ;

foreach ( $matches  as  $match )
	echo ( "Regex::Matches ( {$match [0]}, {$match [1]} ) = " . ( ( Regex::Matches ( $match [0], $match [1] ) ) ? 'true' : 'false' ) . $eol ) ;

echo ( $eol ) ;

// DevelopExpression() example
$expressions	=  
   [
	'file[0-9].txt',			// Gives 'file0.txt' to 'file9.txt'
	'file[a-c][0-2].bin'			// Gives 'filea0.bin' to 'filea2.bin', 'fileb0.bin' to 'fileb2.bin', etc.
    ] ;


foreach (  $expressions  as  $expression )
    {
	echo ( "Regex::DevelopExpression ( $expression ) = $eol$tab" ) ;
	$developed_expressions	=  Regex::DevelopExpression ( $expression ) ;
	echo ( rtrim ( str_replace ( "\n", "$eol$tab", print_r ( $developed_expressions, true ) ) ) ) ;
	echo ( "$eol" ) ;
     }

echo ( $eol ) ;

// PregMatchEx() example
$subject	=  "a:1 b:2" ;
$re		=  '/(?P<sequence> (?P<letter> [a-z]) : (?P<digit> [0-9])) \s (?P<sequence> (?P<letter> [a-z]) : (?P<digit> [0-9]))/imsx' ;

echo ( "Regex::PregMatchEx ( $subject, $re, WIPE ) : " ) ;
$result		=  Regex::PregMatchEx ( $re, $subject, $match, PREG_WIPE_MATCHES | PREG_OFFSET_CAPTURE ) ;
echo ( "\t" . str_replace ( "\n", "$eol$tab", print_r ( $match, true ) ) ) ;
echo ( $eol ) ;

echo ( "Regex::PregMatchEx ( $subject, $re, NOWIPE ) : " ) ;
$result		=  Regex::PregMatchEx ( $re, $subject, $match, PREG_OFFSET_CAPTURE ) ;
echo ( "\t" . str_replace ( "\n", "$eol$tab", print_r ( $match, true ) ) ) ;
echo ( $eol ) ;


// MetaPregMatchEx() example
$regex_list	=
   [
	'1' => '/message start/',
	'2' => '/log: \s* (?P<logmessage> .*)/imsx',
	'3' => '/message end/'    
    ] ;
$sequence	=  '/ \1 \2* \3 /imsx' ;
$lines		=
   [
	'message start',
	'log: this is log message 1',
	'log: this is log message 2',
	'message end'
    ] ;

echo ( "Regex::MetaPregMatchEx ( ) = " ) ;
$status		=  Regex::MetaPregMatchEx ( $sequence, $regex_list, $lines ) ;
echo ( ( $status ) ?  'true' : 'false' ) ;
