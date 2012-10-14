<?php
/*
* Elovalo XPM -> GPERF converter (Russian alphabet)
* CLI script that converts 8x8 XPM files to Elovalo font binary format.
* Prerequisite: a single txt file containing all the glyphs of a wanted alphabet. Can be obtained for example with a well-used imagemagick convert.
*
* Example of creating prerequisite file: 
*	1: Obtain 8x8 bitmap font file of a wanted alphabet (bmp / png)
* 	2: run convert source.png -crop 8x8 -flop cropped/result-%d.xpm
*	3: go to target folder and run cat * > glyphs.txt

* Usage: Change target file name to $input variable
* 	 Write target alphabets to $capitalindex and $lowcase index arrays
*
*	 Run on CLI: php Xpm2Elogperf.php > target.gperf
*	TODO: CLI parameters, flags, formats, output to file.
*/

$input='glyphs.txt';
$output='stuff.c'; 
$capitalindex=array('А','Б','В','Г','Д','Е','Ё','Ж','З','И','Й','К','Л','М','Н','О','П','Р','С','Т','У','Ф','Х','Ц','Ч','Ш','Щ','Ъ','Ы','Ь','Э','Ю','Я');
$lowcaseindex=array('а','б','в', 'г', 'д', 'е', 'ё', 'ж','з','и','й','к','л','м','н','о','п','р','с','т','у','ф','х','ц','ч','ш','щ','ъ','ы','ь','э','ю','я');
$i=0;

$handle = fopen($input, 'r');
$contents = fread($handle, filesize($input));
fclose($handle);

$splosion= explode("/* pixels */",$contents);
//$splosion= explode("None \",",$splosion);

foreach($splosion as $test){
   $second_split= explode("};",$test);
   if($i == 0){	
	 $i++;
   }else
   if($i < (count($capitalindex)+2) && $i>0 ){
       echo "\"".$capitalindex[$i-2]."\",{ ".xpm2bin($second_split[0])."}\n";
   }else{
       echo "\"".$lowcaseindex[($i-2)-count($capitalindex)]."\",{ ".xpm2bin($second_split[0])."}\n";
   }

   $i++;
}

function xpm2bin($dots){
/*
* Input format looks like "...  ...",\n
* Should become 0B000111000,
*/
$formatted=str_replace('".','0B0', $dots);
$formatted=str_replace('.','0',$formatted);
$formatted=str_replace(' ','1',$formatted);
$formatted=str_replace('"','',$formatted);
$formatted=str_replace("\n","",$formatted);
return $formatted;

}

?>
