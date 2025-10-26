<?php
    if (!isset($text) || $text === '') { 
        $text = 'How many inidividuals out of 100 would provide the second word in this pair as their immediate response to the first word? |Type your response on a scale from 0-100 and then press Enter.';
    }

    $texts = explode('|', $text);
    $mainText = array_shift($texts);
?>

<div class="textcenter">
    <div><?php echo isset($text) ? $text : ""; ?></div>

<br>

<div class="study">
    <span class="study-left"   ><?php echo $cue;    ?></span>
    <span class="study-divider"><?php echo ":";     ?></span>
    <span class="study-right"  ><?php echo $answer; ?></span>
    <br>

<br>
  
<div class="textcenter">
    <input name="JOL" type="text" value="" autocomplete="off" class="forceNumeric textcenter collectorInput">
    <button class="collectorButton" id="FormSubmitButton">Submit</button>
</div>
