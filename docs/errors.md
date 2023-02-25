---
layout: default
title: Error Codes
nav_order: 10
---

The following error may occur:

<pre class="terminal-output f9 b9">
<span class="bold"><span class="f1"><span class="f1">[E482] Error:</span></span></span> Variable declaration may be skipped
    <span class="ef246">╭─[</span>tests/samples/invalid/conditional_declaration.pn:16:16<span class="ef246">]</span>
    <span class="ef246">│</span>
 <span class="ef246">10 │</span> <span class="ef249">       </span><span class="f6">goto</span><span class="ef249"> return;</span>
 <span class="ef246">   ·</span>        <span class="f6">──┬─</span>  
 <span class="ef246">   ·</span>          <span class="f6">╰───</span> A jump from this <span class="f6">`goto`</span> statement to '<span class="f5">return</span>'...
 <span class="ef246">   ·</span> 
 <span class="ef246">15 │</span> <span class="ef249">    var </span><span class="f3">result</span><span class="ef249"> = 500i32;</span>
 <span class="ef246">   ·</span>         <span class="f3">───┬──</span>  
 <span class="ef246">   ·</span>            <span class="f3">╰────</span> ...may skip the declaration of the variable '<span class="f3">result</span>'.
 <span class="ef246">16 │</span> <span class="ef249">    </span><span class="f5">return:</span><span class="ef249"> </span><span class="f3">result</span>
 <span class="ef246">   ·</span>     <span class="f5">───┬───</span> <span class="f3">───┬──</span>  
 <span class="ef246">   ·</span>        <span class="f5">╰────────────</span> After this label, the existence of '<span class="f3">result</span>' is dubious.
 <span class="ef246">   ·</span>                <span class="f3">│</span>    
 <span class="ef246">   ·</span>                <span class="f3">╰────</span> The variable is referenced here.
<span class="ef246">────╯</span>

<span class="f1">[E482] Error:</span> Variable declaration may be skipped
    <span class="ef246">╭─[</span>tests/samples/invalid/conditional_declaration.pn:32:21<span class="ef246">]</span>
    <span class="ef246">│</span>
 <span class="ef246">25 │</span> <span class="ef249">       </span><span class="f6">goto</span><span class="ef249"> next;</span>
 <span class="ef246">   ·</span>        <span class="f6">──┬─</span>  
 <span class="ef246">   ·</span>          <span class="f6">╰───</span> A jump from this <span class="f6">`goto`</span> statement to '<span class="f5">next</span>'...
 <span class="ef246">26 │</span> <span class="ef249">    var </span><span class="f3">a</span><span class="ef249"> = 20;</span>
 <span class="ef246">   ·</span>         <span class="f3">┬</span>  
 <span class="ef246">   ·</span>         <span class="f3">╰──</span> ...may skip the declaration of the variable '<span class="f3">a</span>'.
 <span class="ef246">   ·</span> 
 <span class="ef246">30 │</span> <span class="ef249">    </span><span class="f5">next:</span>
 <span class="ef246">   ·</span>     <span class="f5">──┬──</span>  
 <span class="ef246">   ·</span>       <span class="f5">╰────</span> After this label, the existence of '<span class="f3">a</span>' is dubious.
 <span class="ef246">   ·</span> 
 <span class="ef246">32 │</span> <span class="ef249">    result = result + </span><span class="f3">a</span><span class="ef249"> + b;</span>
 <span class="ef246">   ·</span>                       <span class="f3">┬</span>  
 <span class="ef246">   ·</span>                       <span class="f3">╰──</span> The variable is referenced here.
<span class="ef246">────╯</span>
</pre>
