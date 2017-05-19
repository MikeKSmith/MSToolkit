emax.fit<span class="s1">\<- </span><span
class="s2">function</span><span class="s1"> (</span>y<span class="s1">,
</span>dose<span class="s1">, </span>iparm<span class="s1"> =
</span>c<span class="s1">(</span><span class="s3">"ed50"</span><span
class="s1">, </span><span class="s3">"e0"</span><span class="s1">,
</span><span class="s3">"emax"</span><span class="s1">),
</span>ed50cutoff<span class="s1"> = </span><span
class="s4">2.5</span><span class="s1"> \* </span>max<span
class="s1">(</span>doselev<span class="s1">))<span
class="Apple-converted-space"> </span></span>

{

<span class="s1"><span class="Apple-converted-space">   
</span></span>dm<span class="s1"> \<- </span>tapply<span
class="s1">(</span>y<span class="s1">, </span>dose<span class="s1">,
</span>mean<span class="s1">)</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>dsd<span class="s1"> \<- </span>sqrt<span
class="s1">(</span>tapply<span class="s1">(</span>y<span class="s1">,
</span>dose<span class="s1">, </span>var<span class="s1">))</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>intercept<span class="s1"> \<- </span>rep<span
class="s1">(</span><span class="s4">1</span><span class="s1">,
</span>length<span class="s1">(</span>dose<span class="s1">))</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>doselev<span class="s1"> \<- </span>sort<span
class="s1">(</span>unique<span class="s1">(</span>dose<span
class="s1">))</span>

<span class="Apple-converted-space">    </span><span
class="s5">est3</span> \<- <span class="s5">rep</span>(<span
class="s2">NA</span>, <span class="s4">3</span>)

<span class="Apple-converted-space">    </span><span
class="s5">estA</span> \<- <span class="s5">rep</span>(<span
class="s2">NA</span>, <span class="s4">2</span>)

\

<span class="Apple-converted-space">        </span><span
class="s5">e0</span> \<- <span class="s5">min</span>(<span
class="s5">dm</span>)

<span class="Apple-converted-space">        </span><span
class="s5">emax</span> \<- <span class="s5">max</span>(<span
class="s5">dm</span>) - <span class="s5">e0</span>

<span class="s1"><span class="Apple-tab-span"> </span><span
class="Apple-tab-span"> </span></span>ed50<span class="s1"> \<-
</span>max<span class="s1">(</span>doselev<span
class="s1">)/</span><span class="s4">2</span>

<span class="Apple-converted-space">        </span><span
class="s5">Sparm</span> \<- <span class="s5">c</span>(<span
class="s5">ed50</span> = <span class="s5">ed50</span>, <span
class="s5">e0</span> = <span class="s5">e0</span>, <span
class="s5">emax</span> = <span class="s5">emax</span>)

\

<span class="Apple-converted-space">        </span><span
class="s5">fit</span> \<- <span class="s5">try</span>(<span
class="s5">nls</span>(<span class="s5">y</span> \~ <span
class="s5">e0</span> + (<span class="s5">emax</span> \* <span
class="s5">dose</span>)/(<span class="s5">dose</span> + <span
class="s5">ed50</span>),<span class="Apple-converted-space"> </span>

<span class="s1"><span class="Apple-converted-space">           
</span></span>start<span class="s1"> = </span>Sparm<span class="s1">,
</span>control<span class="s1"> = </span>nls.control<span
class="s1">(</span>maxiter<span class="s1"> = </span><span
class="s4">100</span><span class="s1">),<span
class="Apple-converted-space"> </span></span>

<span class="Apple-converted-space">            </span><span
class="s5">trace</span> = <span class="s5">F</span>), <span
class="s5">silent</span> = <span class="s5">T</span>)

\

<span class="s1"><span class="Apple-converted-space">   
</span></span>AcceptConv<span class="s1"> \<- </span>F

<span class="Apple-converted-space">    </span><span
class="s2">if</span> (<span class="s5">class</span>(<span
class="s5">fit</span>) == <span class="s3">"nls"</span>) {

<span class="Apple-converted-space">        </span><span
class="s5">est3</span> \<- <span class="s5">coef</span>(<span
class="s5">fit</span>)

<span class="Apple-converted-space">        </span><span
class="s2">if</span> (<span class="s5">coef</span>(<span
class="s5">fit</span>)[<span class="s4">1</span>] \> <span
class="s4">0</span>) {

<span class="Apple-converted-space">            </span><span
class="s2">if</span> (<span class="s5">coef</span>(<span
class="s5">fit</span>)[<span class="s4">1</span>] \<= <span
class="s5">ed50cutoff</span>) {

<span class="Apple-converted-space">                </span><span
class="s5">vc</span> \<- <span class="s5">as.vector</span>(<span
class="s5">vcov</span>(<span class="s5">fit</span>))

<span class="Apple-converted-space">                </span><span
class="s5">seout</span> \<- <span class="s5">SeEmax3</span>(<span
class="s5">fit</span>, <span class="s5">doselev</span>)

<span class="Apple-converted-space">                </span><span
class="s5">fitpred</span> \<- <span class="s5">seout</span>\$<span
class="s5">fitpred</span>

<span class="Apple-converted-space">                </span><span
class="s5">sdpred</span> \<- <span class="s5">seout</span>\$<span
class="s5">sdpred</span>

<span class="s1"><span class="Apple-tab-span"> </span><span
class="Apple-tab-span"> </span><span class="Apple-tab-span">
</span><span class="Apple-tab-span"> </span></span>sddif<span
class="s1"> \<- </span>seout<span class="s1">\$</span>sddif

<span class="Apple-converted-space">                </span><span
class="s5">AcceptConv</span> \<- <span class="s5">T</span>

<span class="Apple-converted-space">            </span>}

<span class="Apple-converted-space">        </span>}

<span class="Apple-converted-space">    </span>}

<span class="Apple-converted-space">    </span><span
class="s2">if</span> (!<span class="s5">AcceptConv</span>) {

<span class="Apple-converted-space">        </span><span
class="s5">fitL</span> \<- <span class="s5">lm</span>(<span
class="s5">y</span> \~ <span class="s5">dose</span>)

<span class="s1"><span class="Apple-converted-space">       
</span></span>sigL<span class="s1"> \<- </span>summary<span
class="s1">(</span>fitL<span class="s1">)\$</span>sigma

<span class="s1"><span class="Apple-converted-space">       
</span></span>predobj<span class="s1"> \<- </span>predict<span
class="s1">(</span>fitL<span class="s1">, </span>data.frame<span
class="s1">(</span>dose<span class="s1"> = </span>doselev<span
class="s1">), </span>se.fit<span class="s1"> = </span>T<span
class="s1">)</span>

<span class="s1"><span class="Apple-converted-space">       
</span></span>fitpred<span class="s1"> \<- </span>predobj<span
class="s1">\$</span>fit

<span class="s1"><span class="Apple-converted-space">       
</span></span>sdpred<span class="s1"> \<- </span>predobj<span
class="s1">\$</span>se.fit

<span class="s1"><span class="Apple-tab-span"> </span><span
class="Apple-tab-span"> </span></span>sddif<span class="s1"> \<-
</span>doselev<span class="s1"> \* </span>sqrt<span
class="s1">(</span>vcov<span class="s1">(</span>fitL<span
class="s1">)[</span><span class="s4">2</span><span class="s1">,
</span><span class="s4">2</span><span class="s1">])</span>

<span class="Apple-converted-space">    </span>}

<span class="s1"><span class="Apple-converted-space">   
</span></span>return<span class="s1">(</span>list<span
class="s1">(</span>fitpred<span class="s1"> = </span>fitpred<span
class="s1">,<span class="Apple-converted-space"> </span></span>

<span class="s1"><span class="Apple-converted-space">       
</span></span>sdpred<span class="s1"> = </span>sdpred<span class="s1">,
</span>sddif<span class="s1"> = </span>sddif<span class="s1">))</span>

}

\

SeEmax3<span class="s1">\<-</span><span class="s2">function</span><span
class="s1"> (</span>fit<span class="s1">, </span>doselev<span
class="s1">)<span class="Apple-converted-space"> </span></span>

{

<span class="Apple-converted-space">    </span><span
class="s5">E0</span>\<-<span class="s5">coef</span>(<span
class="s5">fit</span>)[<span class="s4">2</span>]

<span class="s1"><span class="Apple-converted-space">   
</span></span>ED50<span class="s1">\<-</span>coef<span
class="s1">(</span>fit<span class="s1">)[</span><span
class="s4">1</span><span class="s1">]</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>EMAX<span class="s1">\<-</span>coef<span
class="s1">(</span>fit<span class="s1">)[</span><span
class="s4">3</span><span class="s1">]</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>fitpred<span class="s1"> \<- </span>E0<span
class="s1">+(</span>EMAX<span class="s1">\*</span>doselev<span
class="s1">)/(</span>ED50<span class="s1">+</span>doselev<span
class="s1">)</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>fitdif<span class="s1"> \<- </span>fitpred<span
class="s1"> - </span>E0

<span class="s1"><span class="Apple-converted-space">   
</span></span>vfcov<span class="s1"> \<- </span>vcov<span
class="s1">(</span>fit<span class="s1">)</span>

<span class="Apple-converted-space">    </span><span
class="s5">sddif</span> \<- <span class="s5">sqrt</span>((<span
class="s5">fitdif</span>\^<span class="s4">2</span>) \* (<span
class="s5">vfcov</span>[<span class="s4">3</span>, <span
class="s4">3</span>]/<span class="s5">coef</span>(<span
class="s5">fit</span>)[<span class="s4">3</span>]\^<span
class="s4">2</span> +<span class="Apple-converted-space"> </span>

<span class="Apple-converted-space">        </span><span
class="s5">vfcov</span>[<span class="s4">1</span>, <span
class="s4">1</span>]/(<span class="s5">coef</span>(<span
class="s5">fit</span>)[<span class="s4">1</span>] + <span
class="s5">doselev</span>)\^<span class="s4">2</span> - (<span
class="s4">2</span> \* <span class="s5">vfcov</span>[<span
class="s4">1</span>,<span class="Apple-converted-space"> </span>

<span class="Apple-converted-space">        </span><span
class="s4">3</span>])/(<span class="s5">coef</span>(<span
class="s5">fit</span>)[<span class="s4">3</span>] \* (<span
class="s5">coef</span>(<span class="s5">fit</span>)[<span
class="s4">1</span>] + <span class="s5">doselev</span>))))

<span class="Apple-converted-space">    </span><span
class="s5">covint</span> \<- -((<span class="s5">doselev</span> \* <span
class="s5">coef</span>(<span class="s5">fit</span>)[<span
class="s4">3</span>])/(<span class="s5">coef</span>(<span
class="s5">fit</span>)[<span class="s4">1</span>] + <span
class="s5">doselev</span>)\^<span class="s4">2</span>) \*<span
class="Apple-converted-space"> </span>

<span class="Apple-converted-space">        </span><span
class="s5">vfcov</span>[<span class="s4">2</span>, <span
class="s4">1</span>] + (<span class="s5">doselev</span>/(<span
class="s5">coef</span>(<span class="s5">fit</span>)[<span
class="s4">1</span>] + <span class="s5">doselev</span>)) \* <span
class="s5">vfcov</span>[<span class="s4">2</span>,<span
class="Apple-converted-space"> </span>

<span class="Apple-converted-space">        </span><span
class="s4">3</span>]

<span class="s1"><span class="Apple-converted-space">   
</span></span>sdpred<span class="s1"> \<- </span>sqrt<span
class="s1">(</span>vfcov<span class="s1">[</span><span
class="s4">2</span><span class="s1">, </span><span
class="s4">2</span><span class="s1">] + </span>sddif<span
class="s1">\^</span><span class="s4">2</span><span class="s1"> +
</span><span class="s4">2</span><span class="s1"> \* </span>covint<span
class="s1">)</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>return<span class="s1">(</span>list<span
class="s1">(</span>fitpred<span class="s1"> = </span>fitpred<span
class="s1">, </span>sdpred<span class="s1"> = </span>sdpred<span
class="s1">, </span>fitdif<span class="s1"> = </span>fitdif<span
class="s1">,<span class="Apple-converted-space"> </span></span>

<span class="Apple-converted-space">        </span><span
class="s5">sddif</span> = <span class="s5">sddif</span>))

}
