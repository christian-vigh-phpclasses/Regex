<?php

// Additional preg_* options
define ( 'PREG_WIPE_MATCHES'		, 0x00010000 ) ;		// Wipes unnamed captures


/**
 * A class that implements extra regex functions.
 *
 * This class provides static methods for :
 * - Converting an Msdos- or Unix-style wildcard string to a regular expression
 * - Matching a string against a wildcard expression
 * - Developing wildcard expressions
 * - Using regular expressions that can have several times the same named captures
 * - Meta-matching groups of lines based on a set of supplied regular expressions
 *
 * @Package	String
 * @Category	Regex
 * @version	1.0
 * @author	Christian Vigh (christian.vigh@orange.fr)
 * 
 */
class Regex
   {
	/**
	 * Returns a set of combinations from an array.
	 * 
	 * Takes an array containing values and nested arrays (no more than one level of nesting is authorized) and 
	 * generates all the possible combinations, each nested array providing alternatives for the generation.
	 *
	 * For example, the following input array :
	 *
	 *	[ [ 'a', 'b' ], 1, 2, [ 'x', 'y', 'z' ] ]
	 *
	 * will generate the following results :
	 *
	 *	[
	 *		[0] => [ 'a', 1, 2, 'x' ] 
	 *		[1] => [ 'b', 1, 2, 'x' ] 
	 *		[2] => [ 'a', 1, 2, 'y' ] 
	 *		[3] => [ 'b', 1, 2, 'y' ] 
	 *		[4] => [ 'a', 1, 2, 'z' ] 
	 *		[5] => [ 'b', 1, 2, 'z' ] 
	 *	 ]
	 *
	 * Note that the combination generation is computed from left to right.
	 *
	 * @param array $array 
	 *	Input array;
	 *	
	 * @param int $max_results 
	 *	This parameter is provided to limit exponential combinations generation.
	 *	
	 * @return array
	 *	Returns an array of arrays containing all the combinations of the supplied input array.
	 */

	// __develop_non_recursive -
	//	A non-recursive version for the CombinationsOf() method, that allows only one nesting level of arrays.
	private static function  __develop_non_recursive ( &$results, $array, $max_results = 10000 )
	   {
		$array_count	=  count ( $array ) ;
		
		// Loop through input array, which is processed from right to left to generate the resulting array
		for  ( $i = $array_count - 1 ; $i  >=  0 ; $i -- )
		   {
			$item		=  $array [$i] ;
			$result_count	=  count ( $results ) ;
			
			// Current item is a nested array : we will have to demultiplicate each result in the $results array
			// to hold each $array items
			if  ( is_array ( $item ) )
			   {
				$item_count	=  count ( $item ) ;
				
				// Check for possible quota overflow
				if  ( $result_count * $item_count  >  $max_results )
					throw ( new Exception ( "Array development limit of $max_results results exceeded." ) ) ;
				
				// Then check that each subitem of this array is not an array in itself
				foreach  ( $item  as  $subitem )
				   {
					if  ( is_array ( $subitem ) )
						throw ( new Exception ( "No more than one nesting level is allowed in arrays to be developed." ) ) ;
				    }
				
				// Demultiplicate existing results
				if  ( $result_count )
				   {
					$new_results	=  [] ;
					
					for  ( $j = 0 ; $j  <  $result_count ; $j ++ )
					   {
						for  ( $k = 0 ; $k  <  $item_count ; $k ++ )
						   {
							$subitem	=  $item [$k] ;
							$new_results []	=  array_merge ( [ $subitem ], $results [$j] ) ;
						    }
					    }
					
					$results	=  $new_results ;
				    }
				// ... but for the first iteration, only add the elements of the current array item
				else
				   {
					foreach  ( $item  as  $subitem )
						$results []	=  [ $subitem ] ;
				    }
			    }
			// Current item is not an array
			else
			   {
				// Prepend it to the existing items
				if  ( $result_count )
				   {
					foreach  ( $results  as  &$result )
						array_unshift ( $result, $item ) ;
				    }
				// ... or simply add it, if the results array is empty
				else
				   {
					$results []	=  [ $item ] ;
				    }
			    }
		    }
	    }
	
	public static function  CombinationsOf ( $array, $max_results = 10000 )
	   {
		$array_count	=  count ( $array ) ;
		
		if  ( ! $array_count )
			return ( [] ) ;

		$results	=  [] ;
		
		self::__develop_non_recursive ( $results, $array, $max_results ) ;
		
		return ( $results ) ;
	    }


	/**
	 * 
	 * Expands a factorized expression.
	 * 
	 * Expands (develops) a factorized string expression.
	 * Sometimes, it is necessary to represent a set of values with a factorized expression,
	 * such as the shell allows us to match a set of files using a pattern.
	 * The DevelopExpression() method allows for input strings that can contain "character
	 * classes" such as :
	 *   
	 *  	[a-c].somestring
	 *  
	 * which will expand to the following array of values :
	 *   
	 *  	a.somestring
	 *  	b.somestring
	 *  	c.somestring
	 *  
	 * Character classes can be either alphabetic or alphanumeric, such as in the following
	 * example :
	 *   
	 *  	[a-b][0-1].somestring
	 *   
	 * which will expand to :
	 *   
	 *  	a0.somestring
	 *  	a1.somestring
	 *  	b0.somestring
	 *  	b1.somestring
	 *   
	 * Numeric values can be zero-padded, using an optional width construct, like in the
	 * following example :
	 *   
	 *  	[a-b][0-1]/4.somestring
	 *   
	 * which will expand to :
	 *   
	 *  	a0000.somestring
	 *  	a0001.somestring
	 *  	b0000.somestring
	 *  	b0001.somestring
	 *   
	 * For alphabetic character classes, the case of the first character determines the case
	 * of the expanded result ; for example :
	 *   
	 *  	[A-c].somestring
	 *   
	 * will give :
	 *   
	 *  	A.somestring
	 *  	B.somestring
	 *  	C.somestring
	 *  
	 * Finally, brackets can be escaped using the backslash character.
	 * 
	 * 
	 * @param string $expression 
	 *	Expression to be developed.
	 *	
	 * @param int $limit 
	 *	Maximum number of developed expressions to be returned. This limit is arbitrarily fixed to 10000.
	 *	When null or negative, no limit is applied.
	 *	
	 * @return array
	 * 	Returns an array containing the developed expression results. The returned value always contain
	 *	at least one element (the supplied input value) when no factorization expression are found.
	 *
	 * @todo
	 *	. Allow for real character classes, such as :
	 *		[A]		(same as [A-A])
	 *		[A-]		(same as [A-Z])
	 *		[-Z]		(same as [A-Z])
	 *		[abc012]	(low/high will cycle through the characters "abc012")
	 *
	 */
	public static function  DevelopExpression ( $expression, $limit = 10000 )
	   {
		// Regular expression that matches :
		// . Either an escaped bracket 
		// . Or a factorization expression
		static $re	=  '#
					(?P<escape> \\\\ [\[\]] ) 
					|
					(?P<range>
						\[
							(?P<rangelo> ( [a-z] | [0-9]+ ) )
							-
							(?P<rangehi> ( [a-z] | [0-9]+ ) )
						\]
						(
							\/
							(?P<width> \d+)
						 )?
					 ) 
				    #imsx' ;
		
		// First simplication case : the input string contain no factorization expression, return
		// it as is
		if  ( ! self::PregMatchAll ( $re, $expression, $matches, PREG_OFFSET_CAPTURE ) )
			return ( [ $expression ] ) ;
		
		// Second elimination case : the input string does not contain any factorization expression,
		// but escaped brackets are present (thus generating a "match")
		$found_range	=  false ;
		
		foreach  ( $matches [ 'range' ]  as  $match )
		   {
			if  ( $match )
			   {
				$found_range	=  true ;
				break ;
			    }
		    }
		
		if  ( ! $found_range )
			return ( [ $expression ] ) ;
		
		// Initializations
		$results		=  [] ;				// The results that will be returned
		$result_count		=  0 ;				// Number of generated results - checked against the $limit parameter
		$ranges			=  [] ;				// Associative arrays that describe each input string portion, either plain constant or factorization expression
		$range_indexes		=  [] ;				// Indexes into $ranges of the entries containing a factorization expression
		$expression_length	=  strlen ( $expression ) ;	// Compute it once and for all
		
		/***
			Loop through the matches found ; upon exit, the $ranges array will contain associative arrays with
			the following keys :
			- 'expression' (boolean) :
				True if the entry is a factorization expression, false otherwise.
			- 'offset' (integer) :
				Start character.
			- 'length' (integer) :
				Length of the expression or plain substring.
			- 'capture' (string) :
		 		Original string (either the plain substring or the factorization expression).
		  
		 	For factorization expressions, the following keys are also present :
		  
		 	- 'low' (integer) :
		 		Low value of the factorization expression. 
		 	- 'high' (integer) :
		 		High value of the factorization expression.
		  
		 	- 'numeric' (boolean) :
		 		True if the factorization expression relates to numeric values, false for alphabetic values.
		  
		 	- 'width' (boolean) :
		 		False for alphabetic values.
		 		For numeric values, specifies the zero-pad width.
		 
		 	- 'current' (integer or string) :
		 		Current index in the generation loop. Initialized to the value of the 'low' entry.
		  
		 ***/
		
		// If first match does not start at string offset zero, create a plain string entry in the $ranges array
		if  ( $matches [ 'range' ] [0] [1]  !=  0 )
			$ranges []	=  
			   [ 
				'expression'	=>  false, 
				'offset'	=>  0, 
				'length'	=>  $matches [ 'range' ] [0] [1],
				'capture'	=>  substr ( $expression, 0, $matches [ 'range' ] [0] [1] )
			    ] ;
			
		// Loop through the matches
		for ( $i = 0 ; $i  <  count ( $matches [ 'range' ] ) ; $i ++ )
		   {
			// $matches [ 'range' ] [$i] will never be an array if an escaped bracket is met
			if  ( ! $matches [ 'range' ] [$i] )
				continue ;
		
			// Create the range entry
			$range	=  
			   [ 
				'expression'	=>  true,
				'low'		=>  $matches [ 'rangelo' ] [$i] [0],
				'high'		=>  $matches [ 'rangehi' ] [$i] [0],
				'offset'	=>  $matches [ 'range'   ] [$i] [1],
				'current'	=>  $matches [ 'rangelo' ] [$i] [0],
				'length'	=>  strlen ( $matches [ 'range' ] [$i] [0] ),
				'capture'	=>  $matches [ 'range' ] [$i] [0],
				'width'		=>  ( $matches [ 'width' ] [$i] ) ?  $matches [ 'width' ] [$i] [0] : false 
			    ] ;
				
			// If low value is alphabetic...
			if  ( ctype_alpha ( $range [ 'low' ] ) )
			   {
				// ... and if high value is also alphabetic
				if  ( ctype_alpha ( $range [ 'high' ] ) )
				   {
					// ... the case of the factorized expressions will be the case of the 'low' value
					if  ( ctype_lower ( $range [ 'low' ] ) )
						$range [ 'high' ]	=  strtolower ( $range [ 'high' ] ) ;
					else
						$range [ 'high' ]	=  strtoupper ( $range [ 'high' ] ) ;
				    }
				// ... but complain if out of alphabetic range
				else
					throw ( new Exception ( "Invalid combination of letters and numbers in factorized expression : " .
							$matches [ 'range' ] [$i] [0] ) ) ;
					
				$range [ 'numeric' ]	=  false ;		// This entry is alphabetic
			    }
			// Low value is not alphabetic : due to the regex construct, this must be an integer, so check that the high
			// value is also an integer
			else if  ( ! is_numeric ( $range [ 'high' ] ) )
					throw ( new Exception ( "Invalid combination of letters and numbers in factorized expression : " .
							$matches [ 'range' ] [$i] [0] ) ) ;
			// Both low and high values are numeric
			else 
				$range [ 'numeric' ]	=  true ;
			
			// Check that low range is not greater than upper range
			if  ( $range [ 'numeric' ] )
			   {
				if  ( ( integer ) $range [ 'low' ]  >  ( integer ) $range [ 'high' ] )
					throw ( new Exception ( "Lower numeric value cannot be greater than upper value in factorized expression : " .
							$matches [ 'range' ] [$i] [0] ) ) ;
			    }
			else if  ( $range [ 'low' ]  >  $range [ 'high' ] )
				throw ( new Exception ( "Lower alphabetic value cannot be greater than upper value in factorized expression : " .
						$matches [ 'range' ] [$i] [0] ) ) ;
				
			// If there is a plain string between the previous range and this one, then we need to add 
			// a plain string entry before it
			$count	=  count ( $ranges ) ;
				
			if  ( $count )		// ... but only if we already have found ranges 
			   {
				$previous_range	=  $ranges [ $count - 1 ] ;
				$last_found	=  $previous_range [ 'offset' ] + $previous_range [ 'length' ] ;  
					
				if  ( $last_found  <  $range [ 'offset' ] )
				   {
					$ranges	[]	=
					   [
						'expression'	=>  false,
						'offset'	=>  $last_found,
						'length'	=>  $range [ 'offset' ] - $last_found,
						'capture'	=>  substr ( $expression, $last_found, $range [ 'offset' ] - $last_found )
					    ] ;
				    }
			    }
			
			// Since the $ranges array contains both plain strings and factorization expressions, we need an array
			// to store factorization expression indexes, in order to simplify the generation process
			$range_indexes []	=  count ( $ranges ) ;
			
			// Add this factorization expression to the list
			$ranges []		=  $range ;
		    }
		
		// If the string does not end with a factorization expression, then we have to add this plain string to
		// the $ranges array
		$count	=  count ( $ranges ) ;
				
		if  ( $count )
		   {
			$previous_range	=  $ranges [ $count - 1 ] ;
			$last_found	=  $previous_range [ 'offset' ] + $previous_range [ 'length' ] ;  
					
			if  ( $last_found  <  $expression_length )
			   {
				$ranges	[]	=
				   [
					'expression'	=>  false,
					'offset'	=>  $last_found,
					'length'	=>  $expression_length - $last_found,
					'capture'	=>  substr ( $expression, $last_found, $expression_length - $last_found )
				    ] ;
			    }
		    }

		// After all this preparatory stuff, we are finally ready to generate the developed values !
		$range_index_count	=  count ( $range_indexes ) ;
		
		while  ( true ) 
		   {
			$current	=  "" ;
			
			// Generate the current developed value
			foreach  ( $ranges  as  $range )
			   {
				if  ( $range [ 'expression' ] )
				   {
					if  ( $range [ 'width' ] )
						$current	.=  sprintf ( "%0{$range [ 'width' ]}d", $range [ 'current' ] ) ;
					else
						$current	.=  $range [ 'current' ] ;
				    }
				else  
					$current	.=  $range [ 'capture' ] ;
			    }
				
			$results []	=  $current ;
			$result_count ++ ;
			
			// Stop if we reached the limit (if any...)
			if  ( $limit  >  0  &&  $result_count  ==  $limit )
				break ;
			
			// This loop finds the next increment value, when multiple factorization expressions are specified
			for  ( $i = $range_index_count - 1 ; $i  >=  0 ; $i -- )
			   {
				// Look only the entries in the $range array that are factorization expressions, starting
				// with the very last one
				$range_index	=  $range_indexes [$i] ;
				$current_range	=  &$ranges [ $range_index ] ;
				
				// Increment the value
				if  ( $current_range [ 'numeric' ] )
					$current_range [ 'current' ]	=  $current_range [ 'current' ] + 1 ;
				else
					$current_range [ 'current' ]	=  chr ( ord ( $current_range [ 'current' ] ) + 1 ) ;
				
				// When the current (last) value reaches the high-end of the range...
				if  ( $current_range [ 'current' ]  >  $current_range [ 'high' ] )
				   {
					// Just start again with the low-end value
					if  ( $i )
						$current_range [ 'current' ]	=  $current_range [ 'low' ] ;
					// ... but only if there are still values to be incremented
					else
						break 2 ;
				    }
				// Current value is still within the [low..high] range
				else
					break ;
			    }
		    }
		
		// All done, return
		return ( $results ) ;
	    }

	
	/**
	 * Allows a regex match result to have captures with the same name.
	 * 
	 * After the result of calling a preg_* function, takes the resulting matches and groups
	 * them using the specified replacement associative array.
	 * The $replacement array comes from a possible call to Regex::RenumberNamedCaptures()
	 * method ; each key defines the renumbered capture names, whereas each value is the
	 * original name. 
	 * There is a 1 -> n relationship between the new and old names, allowing a single
	 * regular expression to contain more than one capture having the same name.
	 * 
	 * @param mixed $match 
	 *	A match result, as can be returned by the self::PregMatch*() functions.
	 *	
	 * @param array $replacements 
	 * An associative array returned by the Regex::RenumberNamedCaptures() method,
	 * having the following shape :
	 *	- key   : New capture name
	 *	- value : old capture name
	 * 
	 * @return array
	 */
	public static function  GroupNamedCaptures ( $match, $replacements )
	   {
		$new_match	=  [] ;
		
		foreach  ( $match  as  $key => $value )
		   {
			// If the key corresponds to one of the matched replacements, group it into an array
			if  ( isset ( $replacements [ $key ] ) )
			   {
				if  ( isset ( $new_match [ $replacements [ $key ] ] ) )
					$new_match [ $replacements [ $key ] ] []	=  $value ;
				else
					$new_match [ $replacements [ $key ] ]		=  [ $value ] ;
			    }
			else
			   {
				if  ( isset ( $new_match [ $key ] ) )
					$new_match [ $key ] []	=  $value ;
				else
					$new_match [ $key ]	=  [ $value ] ;
			    }
		    }
		
		return ( $new_match ) ;
	    }
	
	
	/**
	 * 
	 * Checks if the specified string is a valid regular expression.
	 * 
	 * @param string $str 
	 *	String to be checked.
	 *	
	 * @param string $delimiter 
	 *	Regular expression delimiter. If not specified, the delimiter is taken from
	 *	the first character of the specified input string.
	 *	
	 * @return bool
	 *	Returns true if the specified string represents a regular expression, false otherwise.
	 */
	public static function  IsRegex ( $str, $delimiter = false )
	   {
		static		$pcre_options	=  'imsxe' ;

		$length		=  strlen ( $str ) ;

		// Basic re's must have at least 3 characters : 2 delimiters and a character inside
		if  ( $length  <  3 )
			return ( false ) ;

		// Check that the starting character is not an alphanumeric, backslash or whitespace character
		if  ( $str [0]  ==  '\\'  ||  ctype_alnum ( $str [0] )  ||  ctype_space ( $str [0] ) )
			return ( false ) ;

		// If a delimiter has been specified, check that the supplied string starts with it
		if  ( $delimiter  !==  false )
		   {
			if  ( $str [0]  !=  $delimiter )
				return ( false ) ;
		    }
		// Otherwise, the delimiter will be the first character of the string
		else
			$delimiter	=  $str [0] ;

		// Handle asymetric delimiters
		switch  ( $delimiter ) 
		   {
			case  '<' :  $end_delimiter	=  '>' ; break ;
			case  '{' :  $end_delimiter	=  '}' ; break ;
			case  '[' :  $end_delimiter	=  ']' ; break ;
			case  '(' :  $end_delimiter	=  ')' ; break ;
			default   :  $end_delimiter	=  $delimiter ;
		    }

		// Find the trailing delimiter from the end of the string
		for  ( $i = $length - 1 ; $i  >  0 ; $i -- )
		   {
			$ch	=  $str [$i] ;

			// Delimiter found : we can safely say this is a regex
			if  ( $ch  ==  $end_delimiter )
			   {
				// ... but only if the previous character is not an escape character
				if  ( $i  >  0  &&  $str [ $i - 1 ]  ==  '\\' )
					return ( false ) ;

				return ( true ) ;
			    }
			// But if we find a non-pcre option after the trailing delimiter, then this definitely is not a regex
			else if  ( stripos ( $pcre_options, $ch )  ===  false )
				return ( false ) ;
		    }

		// No trailing delimiter found
		return ( false ) ;
	    }


	/**
	 * 
	 * Checks if a filename corresponds to a filemask.
	 * 
	 * @param string $file 
	 *	Filename to be checked.
	 *	
	 * @param string $pattern 
	 *	Wildcard pattern. See the Regex::WildcardToRegex() method for an explanation on wildcard expressions syntax.
	 *	
	 * @param bool $case_sensitive 
	 *	When true, comparisons are case-sensitive.
	 *	
	 * @return bool|int
	 *	Returns true if $pattern matches $file, false otherwise.
	 *	
	 */
	public static function  Matches ( $file, $pattern, $case_sensitive = false )
	   {
		$length 	= strlen ( $pattern ) ;
		$newpattern 	= "" ;

		for ( $i = 0 ; $i < $length ; $i ++ )
		   {
			$char  = $pattern [ $i ] ;
			$depth = 0 ;

			switch ( $char )
			   {
				case '/' : case '\\' :
					$newpattern	.=  '[\\/\\\\]' ;
					break ;
		
				case '.' : case '+' : case '^' : case '$' : case '(' : case ')' : case '|' :
				case '{' : case '}' : case '=' : case '!' : case '<' : case '>' : case '/' :
					$newpattern .= '\\' . $char ;
					break ;

				case '?' : case '*' :
				        $newpattern .= '[^\\/\\\\]' . $char ;
				        break ;

				case '[' :
				        $newpattern .= '[' ;
				        $depth ++ ;
				        break ;

				case ']' :
				        if  ( ! $depth )
				                return ( false ) ;
					$newpattern .= ']' ;
					break ;

				default :
				        $newpattern .= $char ;
			    }
		    }

		if  ( $case_sensitive )
			$extra = '' ;
		else
			$extra = 'i' ;

		$status		=  preg_match ( '/^' . $newpattern . '$/' . $extra, $file ) ;
		
		return ( ( $status ) ?  true : false ) ;
	    }


	/**
	 * A meta-matching artefact for regular expressions.
	 * 
	 * Suppose you have to scan a sequence of lines, such as in a log file. You want to
	 * recognize which sequence follows which pattern. 
	 *
	 * A sequence in an example log file could be, for example :
	 * - A line containing "message start"
	 * - Any number of lines starting with "log:" and followed by any sequence of characters
	 * - A line containing "message end"
	 *
	 * The following example gives a layout of such a log file :
	 *
	 * 	message start
	 *	log: message 1
	 *	log: message 2
	 *	...
	 *	log: message n
	 *	message end
	 *
	 * The purpose is to check whether a sequence of lines would match this scheme ; a set
	 * of regular expressions would be first needeed to match every particular line in a sequence :
	 *
	 *	$regex_list =
	 *	   [
	 *		'1' => '/message start/',
	 *		'2' => 'log: \s* (?P<logmessage> .*),
	 *		'3' => '/message end/'
	 *	    ] ;
	 *
	 * Then, to match a set of lines containing 'message start', having an unlimited number
	 * of lines starting with 'log:', then ending with a line containing 'message end', you
	 * would have to provide a regular expression using a backreference-style syntax 
	 * referencing the keys of our $regex_list array, which would give :
	 *
	 * 	$sequence	=  '\1 \2* \3' ;
	 *
	 * meaning :
	 *	- The first line must be the one identified by '\1', ie 'message start'
	 *	- There can be any number of lines identified by '\2', ie starting with 'log:'
	 *	- The last line must be 'message end'
	 *
	 * Note that each $regex_list item is a regular expression which can contain group
	 * captures, either named or not.
	 * If it does not contain re delimiters, then '/ /imsx' is assumed, so do not forget that
	 * spaces will not be significant.
	 *
	 * Thus, checking if a set of lines (in an array) matches the regular expressions 
	 * specified in $sequence and defined in $regex_list, a simple call will be enough :
	 *
	 *  	$status = Regex::MetaPregMatchEx ( $sequence, $regex_list, $lines ) ;
	 * 
	 * @param string $sequence 
	 * 	A regular expression containing preg backreference-style constructs that
	 *	refer to array keys in the $regex_list array.
	 *
	 *	The following preg-style backreferences are supported ('x' stands for a 
	 *	sequence of digits, 'name' for a group capture name) :
	 *	- \x
	 *	- \gx
	 *	- \g{x}
	 *	- (?P=name)
	 *	- \k<name>
	 *	- \k'name'
	 *	- \k{name}
	 *	- \g{name}
	 * 
	 * @param array $regex_list 
	 *	An associative array whose keys are backreference ids (either the 'x' or the
	 *	'name' string described in the $sequence parameter help) and whose values
	 *	are regular expressions.
	 *	Each entry is meant to match one or more lines of a sequence of lines.
	 *
	 *	If no delimiter encloses the regex, then a default delimiter '/' will be used, 
	 *	and the 'imsx' preg options will be automatically added before performing the 
	 *	match.
	 *
	 * 
	 * @param array $subject_array 
	 *	Array of input lines to be matched against the specified sequence.
	 *	
	 * @param array $matches 
	 *	Reference to an array which will receive the individual matches.
	 *	Each entry is an associative array having the following keys :
	 *	- 'reference' :
	 *		The original string reference.
	 *	- 'regex' :
	 *		The regex that matched the line.
	 *	- 'matches' :
	 *		Array of matches. Note that since the method uses PregMatchEx(),
	 *		an additional level of indirection is added with regards to self::PregMatch,
	 *		since several captures can have the same name.
	 *
	 * @param int $flags 
	 *	PREG_* Flags for the self::PregMatch*() function.
	 *	
	 * @param array $missing_matches 
	 *	When specified, the indexes of non-matching lines will be stored in this array.
	 * 
	 * @return bool
	 *	Returns true if the sequence of lines matches the specified sequence, false otherwise.
	 */
	public static function  MetaPregMatchEx ( $sequence, $regex_list, $subject_array, &$matches = null, $flags = 0, $match_all = false, $missing_matches = [] )
	   {
		$match_list		=  [] ;			// List of matched results in $subject_array
		$match_references	=  [] ;			// List of corresponding match references (keys in regex_list array)
		
		// Normalize the sequence
		$new_sequence		=  self::NormalizeMetaSequence ( $sequence ) ;
		
		$subject_index		=  0 ;
		
		// Loop through each log line
		foreach  ( $subject_array  as  $subject )
		   {
			$found	=  false ;
			
			// Compare with each item in regex list
			$regex_index	=  0 ;
			
			foreach  ( $regex_list  as  $regex_key => $regex_value )
			   {
				// Normalize regular expression
				if  ( ! self::IsRegex ( $regex_value ) )
					$regex_value	=  '/^ \s* ' . str_replace ( '/', '\\/', $regex_value ) . '/imsx' ;

				// Call either the PregMatchEx or PregMatchAllEx method
				if  ( $match_all )
					$status		=  self::PregMatchAllEx ( $regex_value, $subject, $match_result, $flags ) ;
				else
					$status		=  self::PregMatchEx ( $regex_value, $subject, $match_result, $flags ) ;
				
				// Match found : add this line to the match list
				if  ( $status )
				   {
					$reference		=  '\\' . $regex_key ;
					$new_match		=  
					   [ 
						'reference'	=>  $regex_key, 
						'subject'	=>  $subject, 
						'subject-index' =>  $subject_index,
						'regex'		=>  $regex_value, 
						'regex-index'	=>  $regex_index,
						'matches'	=>  $match_result 
					    ] ;
					
					// Handle the case where several regex match the same string
					// For example, if both regex indexed by "\4" and "\5" match the same string, we will have
					// to construct a matching regex having the following contents :
					//	( (\4) | (\5) )
					if  ( isset ( $match_list [ $subject_index ] ) )
					   {
						$match_list [ $regex_key ] []		=  $new_match ;
						$match_references [ $subject_index ] []	=  $reference ;
					    }
					else
					   {
						$match_list [ $regex_key ]		=  [ $new_match ] ;
						$match_references [ $subject_index ]	=  [ $reference ] ;
					    }
					
					$found			=  true ;
				    }
				
				$regex_index ++ ;
			    }
			
			// If no matching regex has been found, record the index of the current line
			if  ( ! $found )
				$missing_matches []	=  $subject_index ;
			
			$subject_index ++ ;
		    }

		// Not all subject lines have a match in the $regex_list array : consider this match has failed
		$status		=  false ;
		
		if  ( ! count ( $missing_matches ) )
		   {
			// Since an input string may correspond to several matches, get all the possible combination of matches
			$key_subjects	=  self::CombinationsOf ( $match_references ) ;
		
			// Make $sequence a regex if not already specified : add delimiters and options
			if  ( ! self::IsRegex ( $new_sequence ) )
				$new_sequence	=  '/^ ' . str_replace ( '/', '\\/', $new_sequence ) . ' $/imsx' ;

			// Find a match
			$status		=  false ;
		
			foreach  ( $key_subjects  as  $key_subject )
			   {
				$subject_string		=  implode ( '', $key_subject ) ;
			
				if  ( self::PregMatchAll ( $new_sequence, $subject_string, $seq_match ) )
				   {
					$matches	=  $match_list ;
					$status		=  true ;
				    }
			     }
		     }
		
		// Return match status
		return ( $status ) ;
	    }

	
	/**
	 * Performs multiple inline substrings replacements.
	 * 
	 * @param string $subject 
	 *	String where replacements are to be performed.
	 *	
	 * @param array $replacements 
	 * 	 Array of arrays, each of them containing 3 elements :
	 *	- Element 1 :
	 *		The string to be replaced in $subject.
	 *	- Element 2 :
	 *		The replacement string.
	 *	- Element 3 :
	 *		The offset, in $subject, of the string to be replaced.
	 * 
	 * @return string
	 *	The input string, with all replacements having taken place.
	 *	
	 */
	public static function  MultiSubstrReplace ( $subject, $replacements )
	   {
		// First, sort entries by ascending offset
		if  ( count ( $replacements )  >  1 )
			usort ( $replacements, function ( $a, $b ) { return ( $a [2] - $b [2] ) ; } ) ;
		
		// Parts that are extracted from the subject
		$list			=  [] ;
		
		// Last seen subject string offset so far
		$last_seen_offset	=  0 ;
		$subject_length		=  strlen ( $subject ) ;
		
		// Loop through replacement strings
		foreach  ( $replacements  as  $replacement )
		   {
			$length			=  strlen ( $replacement [0] ) ;

			// Delta between current offset and last seen offset : add this substring to the list of items to be joined at the end
			if  ( $replacement [2]  >  $last_seen_offset )
			   {
				$list []		=  substr ( $subject, $last_seen_offset, $replacement [2] - $last_seen_offset ) ;
				$last_seen_offset       =  $replacement [2] + $length ;
			    }
			
			// Include the replacement string
			$list []		=  $replacement [1] ;
			
			// Update currently last seen offset (= current offset + current string length)
			$last_seen_offset	=  $replacement [2] + $length ;
		    }
		
		// Don't forget the trailing characters
		if  ( $last_seen_offset  <  $subject_length )
			$list []	=  substr ( $subject, $last_seen_offset ) ;
		
		// All done, return
		return ( implode ( '', $list ) ) ;
	    }

	
	/**
	 * 
	 * Normalizes a sequence for the MetaPregMatchEx method.
	 * 
	 * Normalizes a meta-sequence, which uses preg-like backreference syntax to reference 
	 * regular expressions indexed by the backreference value in the $match_definitions array.
	 * The method accepts all the backreference syntaxes that are recognized by the preg_replace
	 * function ('x' stands for a sequence of digits, 'name' for a group capture name) :
	 *	- \x
	 *	- \gx
	 *	- \g{x}
	 *	- (?P=name)
	 *	- \k<name>
	 *	- \k'name'
	 *	- \k{name}
	 *	- \g{name}
	 * All those forms are normalized in the input sequence as :
	 *	(\x)
	 * or :
	 *	(\name)
	 * Note the enclosing parentheses to prevent side effects when performing the match.
	 * 
	 * @param string $sequence 
	 *	Sequence to be normalized.
	 *	
	 * @param mixed $subsequences 
	 *	Associative array whose keys are sequence references and whose values are
	 *	regular expressions to be matched.
	 *	When specified, all references specified in the $sequence parameter are checked
	 *	for existence in this array.
	 *	
	 * @return string
	 *	Returns the normalized sequence.
	 *	
	 */

	// A regular expression matching all the possible backreferences allowed in PHP 
	// ('x' stands for a sequence of digits, 'name' for a group capture name) :
	// - \x
	// - \gx
	// - \g{x}
	// - (?P=name)
	// - \k<name>
	// - \k'name'
	// - \k{name}
	// - \g{name}
	// Because of the (?P=name) form, we cannot unify all the regular expressions matching the
	// above syntaxes, so we need to split the matching in two groups		
	const	META_SEQUENCE_MATCH	=  '# [^\\\\]?
					      (?P<match>
						(?P<group1>
							\\\\
							(
								(
									g (?P<number_reference_2> \d+)
								 )
								|
								(
									g \{ (?P<number_reference_3> \d+) \}
								 )
								|
								(
									k \< (?P<name_reference_1> [\w.\-]+) \>
								 )
								|
								(
									k \' (?P<name_reference_2> [\w.\-]+) \'
								 )
								|
								(
									[kg] \{ (?P<name_reference_3> [\w.\-]+) \}
								 )
								|
								(
									(?P<name_reference_4> [\w.\-]+)
								 )
								|
								(
									(?P<number_reference_1> \d+)
								 )
							 )
						 )
						|
						(?P<group2>
							\( \? P = (?P<named_reference> [\w_.\-]+) \)
						 )
					      )
					    #imsx' ;

	public static function  NormalizeMetaSequence ( $sequence, &$subsequences = null )
	   {
		// Are there any references to match items in the sequence string ?
		if  ( preg_match_all ( self::META_SEQUENCE_MATCH, $sequence, $matches, PREG_OFFSET_CAPTURE ) )
		    {
			$substrings	=  [] ;

			// Loop through found matches
			foreach  ( $matches  as  $key => $match )
			   {
				// Numeric keys simply indicate unnamed group captures - ignore them
				if  ( is_numeric ( $key ) )
					continue ;
				
				// In order to obtain the real capture length, we need to determine whether the match comes
				// from <group1> or <group2>
				$group		=  false ;
				
				if  ( ! strncmp ( $key, 'number', 5 )  ||  ! strncmp ( $key, 'name', 4 ) )
					$group	=  '1' ;
				else if  ( ! strncmp ( $key, 'named', 5 ) )
					$group	=  '2' ;
				else
					continue ;

				$groupname	=  "group$group" ;
				
				$index		=  0 ;

				// Loop through each match found
				foreach  ( $matches [ $key ]  as  $value )
				   {
					// ... but only if the value is an array and its offset item is not negative
					if  ( is_array ( $value )  &&  $value [1]  !=  -1 )
					   {
						// Add it to the substrings array which will be passed to the Regex::MultiSubstrReplace() method
						// Note the extra 4th element, not used by Regex::MultiSubstrReplace, which serves to determine
						// if the reference exists as a key in the $match_definitions array, when specified
						$substrings []	=  
						   [ 
							$matches [ 'match' ] [ $index ] [0], 
							'(\\\\' . $value [0] . ')', 
							$matches [ $groupname ] [ $index ] [1], 
							$matches [ $key ] [ $index ] [0]
						    ] ;
					    }
				    
					   $index ++ ;
				    }
			    }

			// Replace all reference syntaxes allowed with a much simpler "(\reference)" form 
			$new_sequence	=  Regex::MultiSubstrReplace ( $sequence, $substrings ) ;

			// If matches have been specified, check that all the references found in $sequence have a corresponding entry
			// in the $match_definitions array
			if  ( $subsequences  !==  null )
				$subsequences	=  $substrings ;
			
			return ( $new_sequence ) ;
		     }
		// Useless sequence, since it contains no reference to a match string
		else 
			throw new Exception ( "The following sequence does not contain any reference to match strings :\n$sequence" ) ;
	    }
	
	
	/**
	 * 
	 * Removes unnamed captures from the result of a call to a preg_* function.
	 * 
	 * @param array $matches 
	 *	Matches returned as the 3rd parameter of a preg_* call.
	 *	
	 * @param int $flags 
	 *	PREG_* flags specified when calling a preg_* function. When the PREG_OFFSET_CAPTURE flag is
	 *	specified, it happens that some results are an empty string instead of a 2-elements array.
	 *	This helps removing unnecessary entries.
	 * 
	 */
	public static function  PregWipeMatches ( &$matches, $flags )
	   {
		$new_matches	=  [] ;
		
		foreach  ( $matches  as  $key => $value )
		   {
			if  ( is_numeric ( $key ) )
				continue ;
			
			if  ( $flags  &  PREG_OFFSET_CAPTURE )
			   {
				if  ( ! is_array ( $value ) )
					continue ;
			
				if  ( $value [1]  ==  -1 )
					continue ;
			    }
			
			$new_matches [ $key ]	=  $value ;
		    }
		
		$matches	=  $new_matches ;
	    }
	
	
	/**
	 * 
	 * Encapsulates the preg_match() function and optionnally wipes unnamed captures if the PREG_WIPE_MATCHES flag
	 * is specified.
	 * 
	 */
	public static function  PregMatch ( $pattern, $subject, &$matches = null, $flags = 0, $offset = 0 )
	   {
		// Perform the match and handle potential error
		$status		=  @preg_match ( $pattern, $subject, $matches, $flags, $offset ) ;
		
		if  ( $status  ===  false )
			throw ( new Exception ( "Invalid regex : $pattern" ) ) ;
		
		// If needed, wipe any unnamed captures
		if  ( $flags  &  PREG_WIPE_MATCHES )
			self::PregWipeMatches ( $matches, $flags ) ;
		
		// All done, return
		return ( $status ) ;
	    }

	
	/**
	 * 
	 * Encapsulates the preg_match_all() function and optionnally wipes unnamed captures if the PREG_WIPE_MATCHES flag
	 * is specified.
	 * 
	 */
	public static function  PregMatchAll ( $pattern, $subject, &$matches = null, $flags = PREG_PATTERN_ORDER, $offset = 0 )
	   {
		// Perform the match and handle potential error
		$status		=  @preg_match_all ( $pattern, $subject, $matches, $flags, $offset ) ;
		
		if  ( $status  ===  false )
			throw ( new Exception ( "Invalid regex : $pattern" ) ) ;
		
		// If needed, wipe any unnamed captures
		if  ( $flags  &  PREG_WIPE_MATCHES )
			self::PregWipeMatches ( $matches, $flags ) ;
		
		// All done, return
		return ( $status ) ;
	    }

	
	/**
	 * 
	 * Encapsulates the preg_replace() function.
	 * 
	 */
	public static function  PregReplace ( $pattern, $replacement, $subject, $limit = -1, $count = null )
	   {
		$status		=  @preg_replace ( $pattern, $replacement, $subject, $limit, $count ) ;
		
		if  ( $status  ===  false )
			throw ( new Exception ( "Invalid regex : $pattern" ) ) ;
		
		return ( $status ) ;
	    }
	
	
	/**
	 * 
	 * An extended version of the preg_match() function, that allows for specifying multiple named captures 
	 * with the same name.
	 * 
	 * @param array $matches
	 * 	Strings matched by the captures. Since named captures can be specified more than once, each array item 
	 * 	will contain an additional level of indirection, an array for each matched item. 
	 * 	Thus, the elements of a capture named <pat> will be accessible through the following expressions :
	 *	- $match [ 'pat' ] [0] will yield to the first capture of a group named 'pat'
	 *	- count ( $match [ 'pat' ] ) will give the number of expressions matched by the named capture 'pat'
	 *	The same applies to unnamed captures.
	 *
	 */
	public static function  PregMatchEx ( $pattern, $subject, &$matches = null, $flags = 0, $offset = 0 )
	   {
		$newpattern	=  self::RenumberNamedCaptures ( $pattern, $replacements ) ;

		if  ( $status = self::PregMatch ( $newpattern, $subject, $matches, $flags, $offset ) )
		   {
			$matches	=  self::GroupNamedCaptures ( $matches, $replacements ) ;
		    }
		
		return ( $status ) ;
	    }

	
	/**
	 * 
	 * An extended version of the preg_match_all() function, that allows for specifying multiple named captures 
	 * with the same name.
	 * 
	 */
	public static function  PregMatchAllEx ( $pattern, $subject, &$matches = null, $flags = 0, $offset = 0 )
	   {
		$replacements	=  [] ;
		$newpattern	=  self::RenumberNamedCaptures ( $pattern, $replacements ) ;
		
		if  ( $status = self::PregMatchAll ( $newpattern, $subject, $matches, $flags, $offset ) )
		   {
			$matches	=  self::GroupNamedCaptures ( $matches, $replacements ) ;
			
			// Cancel one level of indirection in the results ; the resulting match array will have
			// the same shape as the one returned by the PregMatchEx() method
			foreach  ( $matches  as  $key => $value )
			   {
				// Non-numeric keys are named pattern matches : get rid of one indirection level and collect only
				// the successful matches ; this is a deviation from the normal preg_match_all() function, where the
				// results could include as well an empty string or a sub-array with an empty string and an offset of -1
				if  ( ! is_numeric ( $key ) )
				   {
					$new_array	=  [] ;
					
					foreach  ( $value  as  $subvalue )
					   {
						foreach  ( $subvalue  as  $subsubkey => $subsubvalue )
						   {
							if  ( isset ( $subsubvalue )  &&  is_array ( $subsubvalue )  &&  $subsubvalue [1]  !=  -1 )
								$new_array [ $subsubkey ]	=  $subsubvalue ;
						    } 
					    }
					
					$matches [ $key ]	=  $new_array ;
				    }
				// For numeric keys, when the sub-array contains only one element, then this is a normal preg_match result,
				// so get rid of the level of indirection that has been added by the GroupNamedCaptures() method
				else if  ( count ( $value )  ==  1 )
					$matches [ $key ]	=  $value [0] ;
			    }
		    }
		
		return ( $status ) ;
	    }

	
	/*--------------------------------------------------------------------------------------------------------------
	
	    NAME
	        PregStrReplace - Replace string(s) using regular expression(s)
	
	    PROTOTYPE
	        $result		=  PdfToText::PregStrReplace ( $pattern, $replacement, $subject, $limit = -1, 
						&$match_count = null )
	
	    DESCRIPTION
	        This function behaves like a mix of str_replace() and preg_replace() ; it allows to search for strings
		using regular expressions, but the replacements are plain-text strings and no reference to a capture
		specified in the regular expression will be interpreted.
		This is useful when processing replacement strings that contain constructs such as "\00" or "$", which 
		are interpreted by preg_replace() as references to captures.

		This function has the same parameters as preg_replace().
	
	    RETURN VALUE
	        Returns the substituted text.
	
	 *-------------------------------------------------------------------------------------------------------------*/
	public static function  PregStrReplace ( $pattern, $replacement, $subject, $limit = -1, &$match_count = null )
	   {
		// Make sure that $pattern and $replacement become arrays of the same size
		if  ( is_array ( $pattern ) )
		   {
			if  ( is_array ( $replacement ) )
			   {
				if  ( count ( $pattern )  !==  count ( $replacement ) )
				   {
					trigger_error ( "The \$replacement parameter should have the same number of element as \$pattern." ) ;
					return ( $subject ) ;
				    }
			    }
			else
				$replacement	=  array_fill ( $replacement, count ( $pattern ), $replacement ) ;
		    }
		else 
		   {
			if  ( is_array ( $replacement ) )
			   {
				trigger_error ( "Expected string for the \$replacement parameter." ) ;
				return ( $subject ) ;
			    }

			$pattern	=  array ( $pattern ) ;
			$replacement	=  array ( $replacement ) ;
		    }

		// Upper limit
		if  ( $limit  <  1 )
			$limit		=  PHP_INT_MAX ;

		// Loop through each supplied pattern
		$current_subject	=  $subject ;
		$count			=  0 ;
		
		for  ( $i = 0, $pattern_count = count ( $pattern ) ; $i  <  $pattern_count ; $i ++ )
		   {
			$regex		=  $pattern [$i] ;

			// Get all matches for this pattern
			if  ( preg_match_all ( $regex, $current_subject, $matches, PREG_OFFSET_CAPTURE ) )
			   {
				$result		=  '' ;		// Current output result
				$last_offset	=  0 ;

				// Process each match
				foreach  ( $matches [0]  as  $match )
				   {
					$offset		=  ( integer ) $match [1] ;

					// Append data from the last seen offset up to the current one
					if  ( $last_offset  <  $offset )
						$result		.=  substr ( $current_subject, $last_offset, $offset - $last_offset ) ;

					// Append the replacement string for this match
					$result		.=  $replacement [$i] ;

					// Compute next offset in $current_subject
					$last_offset     =  $offset + strlen ( $match [0] ) ;

					// Limit checking
					$count ++ ;

					if  ( $count  >  $limit )
						break 2 ;
				    }

				// Append the last part of the subject that has not been matched by anything
				$result			.=  substr ( $current_subject, $last_offset ) ;

				// The current subject becomes the string that has been built in the steps above
				$current_subject	 =  $result ;
			    }
		    }

		/// All done, return
		return ( $current_subject ) ;
	    }


	/**
	 * 
	 * Gives a unique id to each named capture within a regex.
	 * 
	 * Reassigns unique identifiers to named captures within a regular expression. The new
	 * identifiers will have the form "prefix_x", where "prefix" is given by the $prefix
	 * parameter, and "x" a unique identifier starting from 0.
	 * 
	 * @param string $pattern 
	 *	Regex pattern containing potential named captures to be renamed.
	 * 
	 * @param array $correspondances 
	 *	On output, will hold an associative array whose keys are the new capture group names, and values the old ones.
	 * 
	 * @param string $prefix 
	 *	Prefix string for capture name replacements.
	 * 
	 * @return string
	 *	Returns the input pattern with all named captures replaced by unique identifiers.
	 *	
	 * @notes
	 *	This method, along with the GroupNamedCaptures() one, is used by the Preg*Ex methods
	 *	to allow processing of regular expressions having duplicate named captures.
	 *	
	 */
	public static function  RenumberNamedCaptures ( $pattern, &$correspondances = [], $prefix = 'match_' )
	   {
		static	$re	=  '/
					\( \? P < 
						(?P<pattern> [^>]+ )
					>
				    /imsx' ;
		
		// Get named captures
		if  ( self::PregMatchAll ( $re, $pattern, $matches, PREG_OFFSET_CAPTURE ) )
		   {
			$index			=  0 ;
			$pattern_matches	=  [] ;
			
			// Loop through pattern matches
			foreach  ( $matches [ 'pattern' ]  as  $match ) 
			   {
				$pname					=  $match [0] ;
				$poffset				=  $match [1] ;
				$newpattern				=  "$prefix$index" ;
				
				// Build the correspondance array
				$correspondances [ $newpattern ]	=  $pname ;
				
				// Add this entry (old name, new name, offset) into an array for the Regex::MultiSubsrReplace() method
				$pattern_matches []			=  [ $pname, $newpattern, $poffset ] ;
				$index ++ ;
			    }
			
			// Perform the multiple-string replace
			$new_pattern	=  Regex::MultiSubstrReplace ( $pattern, $pattern_matches ) ;
		    }
		else
			$new_pattern	=  $pattern ;

		// All done, return
		return ( $new_pattern ) ;
	    }
	
	
	/**
	 * 
	 * Replaces patterns in a regular expression string.
	 * 
	 * Replace named patterns in a string. This function uses the result of self::PregMatchAll()
	 * to match named patterns with the supplied input array $replacements.
	 *  
	 * @param string $pattern 
	 *	A pattern matching subpart(s) of the specified subject string.
	 *	
	 * @param string $subject 
	 *	String to be matched against.
	 * 
	 * @param array $replacements 
	 *	Associative array whose keys are the pattern name (as specified in the 
	 *	(?P<name> re) parts of a regular expression) and whose values are also 
	 *	an associative array. Each entry in the array have the following meaning :
	 *
	 *	- key     : A regular expression specifying the value of the named pattern
	 *		    name. Do not put anchors nor delimiters in this pattern since
	 *		    they are automatically added.
	 *	- value	  : The replacement value for the named pattern specified by the key
	 *		    value.
	 * 
	 * @param int $options 
	 *	PREG_* options.
	 *	
	 * @return string
	 *	Returns the substituted string.
	 *	
	 * @example
	 *	The following example will replace :
	 *	- every matched pattern named "toto" by one of the string matching this pattern,
	 *	  either 'the new replacement for the pattern string' or 'the replacement of thepattern',
	 *	  for captures matching either 'pattern.*' or 'thepattern', respectively.
	 *	  The (?P<pattern1> ...) or (?P<pattern2> ...) match the first item.
	 *	- Every matched pattern name "aaa" by the string 'replacement of aaa'.
	 *
	 *	$subject	=  "aaa (?P<pattern1> coucou) zzzz (?P<pattern2> coucou2) aaa (?P<thepattern> zzzz)";
	 *	$pattern	=  "# ( \( \? P < (?P<toto> [^>]+) > .*? \) ) | (?P<aaa> aaa*) #imsx" ;
	 *	$replacement	= 
	 *	   [
	 *		"toto"	=>  
	 *		   [
	 *			'pattern.*'	=>  'the new replacement for the pattern string',
	 *			'thepattern'	=>  'the replacement of thepattern'
	 *		    ],
	 *		"aaa"	=> 
	 *		   [
	 *			'aaa'		=>  'replacement of aaa'
	 *		    ]
	 *	    ] ;
	 *	$result		=  String::RegReplaceNamedPatterns ( $pattern, $subject, $replacement ) ) ;
	 * 
	 *	The resulting string will be :
	 *		replacement of aaa (?P<the new replacement for the pattern string> coucou) zzzz (?P<the
	 *		new replacement for the pattern string> coucou2) replacement of aaa (?P<the replacement of thepattern> zzzz)
	 *
	 */
	public static function  ReplaceNamedPatterns ( $pattern, $subject, $replacements, $options = null )
	   {
		// Process the substitutions only if there is a match...
		if  ( self::PregMatchAll ( $pattern, $subject, $matches, PREG_OFFSET_CAPTURE | $options ) )
		   {
			$substitutions	=  [] ;
			
			// Loop through matches
			foreach  ( $matches  as  $match_key => $match_values )
			   {
				// ... then through each matched value
				foreach  ( $match_values  as  $match_value ) 
				   {
					// Ignore empty matches
					if  ( ! $match_value  ||  $match_value [1]  ==  -1 )
						continue ;
					
					// Loop through replacement patterns
					foreach  ( $replacements  as  $replacement_key => $replacement_values )
					   {
						// Skip non-matching ones
						if  ( strcmp ( $match_key, $replacement_key ) )
							continue ;
							
						// Find the replacement values for each matched pattern
						foreach  ( $replacement_values  as  $replacement_re => $replacement_value )
						   {
							if  ( self::PregMatch ( "/^$replacement_re\$/imsx", $match_value [0] ) )
							   {
								$substitutions []	=
								   [
									$match_value [0],		// Original value to be replaced
									$replacement_value,		// Replacement value
									$match_value [1]		// Original value offset
								    ] ;
							    }
						    }
					    }
				    }
			    }
			
			// Replace catched captures
			$new_subject	=  self::MultiSubstrReplace ( $subject, $substitutions ) ;
			
			return ( $new_subject ) ;
		    }
		// No match found : return the subject as is
		else
			return ( $subject ) ;
	    }


	/**
	 * Converts an Msdos or Unix-style wildcard string to a regular expression.
	 * 
	 * Converts an Msdos or Unix-style wildcard string to a regular expression. Allowed wildcard forms are :
	 * '?' -
	 *	Matches 0 or 1 character  Path separator cannot be matched with this expression.
	 * '*' -
	 *	Matches 0 or more characters. Path separator cannot be matched with this expression.
	 * '[cclass]' -
	 *	A character class that matches one character, for example "[a-z]" to match any lowercase alphabetic character, or
	 *	"[^a-z]" to match anything but a lowercase alphabetic character.
	 *	
	 * Note that the path separator can either be '/' or '\' : this method does not take care of the host operating system conventions.
	 * 
	 * @param string	$pattern 
	 *	Wildcard pattern to be converted.
	 *	
	 * @param string	$escaped_chars 
	 *	Optional characters to be escaped in the regular expression (regex special characters are systematically escaped).
	 *	
	 * @return		bool|string 
	 *	Returns false if the wilcard string cannot be converted to a regular expression (typically because there is an angle 
	 *	bracket mismatch) or the regular expression corresponding to the supplied wildcard pattern.
	 *	Note that the returned value does not include the leading and trailing delimiters, so that it can be included into
	 *	an existing regex.
	 *	
	 */
	public static function WildcardToRegex ( $pattern, $escaped_chars = "" ) 
	   {
		$length 	= strlen ( $pattern ) ;
		$newpattern 	= "" ;

		for ( $i = 0 ; $i < $length ; $i ++ )
		   {
			$char  = $pattern [ $i ] ;
			$depth = 0 ;

			switch ( $char )
			   {
				case '.' : case '+' : case '^' : case '$' : case '(' : case ')' : case '|' :
				case '{' : case '}' : case '=' : case '!' : case '<' : case '>' : case '/' :
					$newpattern .= '\\' . $char ;
					break ;

				case '?' : case '*' :
				        $newpattern .= '[^\\/]' . $char ;
				        break ;

				case '[' :
				        $newpattern .= '[' ;
				        $depth ++ ;
				        break ;

				case ']' :
				        if  ( ! $depth )
				                return ( false ) ;
					
					$newpattern .= ']' ;
					break ;

				default :
					if  ( strpos ( $escaped_chars, $char )  !==  false )
						$newpattern	.=  '\\' ;
					
				        $newpattern .= $char ;
			    }
		    }

		return ( $newpattern ) ;
	    }
    }
