\

\# MSToolkit will create ReplicateData, MicroData and MacroData
directories<span class="Apple-converted-space"> </span>

\# under the current working directory

getwd()

\

library<span class="s1">(</span>MSToolkit<span class="s1">)</span>

\

generateData<span class="s1">( </span>replicateN<span class="s1"> =
</span><span class="s2">5</span><span class="s1">, </span>subjects<span
class="s1"> = </span><span class="s2">100</span><span class="s1">,
</span>treatDoses<span class="s1"> = </span>c<span
class="s1">(</span><span class="s2">0</span><span class="s1">,
</span><span class="s2">5</span><span class="s1">, </span><span
class="s2">10</span><span class="s1">, </span><span
class="s2">50</span><span class="s1">, </span><span
class="s2">100</span><span class="s1">),<span
class="Apple-converted-space"> </span></span>

<span class="s1"><span class="Apple-converted-space"> 
</span></span>genParNames<span class="s1"> = </span><span
class="s3">"E0,ED50,EMAX"</span><span class="s1">,
</span>genParMean<span class="s1"> = </span>c<span
class="s1">(</span><span class="s2">2</span><span
class="s1">,</span><span class="s2">50</span><span
class="s1">,</span><span class="s2">10</span><span class="s1">),
</span>genParVCov<span class="s1"> = </span>c<span
class="s1">(</span>.5<span class="s1">,</span><span
class="s2">30</span><span class="s1">,</span><span
class="s2">10</span><span class="s1">),<span
class="Apple-converted-space"> </span></span>

<span class="s1"><span class="Apple-converted-space"> 
</span></span><span class="s4">respEqn</span><span class="s1"> =
</span>"E0 + ((DOSE \* EMAX)/(DOSE + ED50))"<span class="s1">,<span
class="Apple-converted-space">  </span></span><span
class="s4">respVCov</span><span class="s1"> = </span><span
class="s2">2</span><span class="s1">,<span
class="Apple-converted-space"> </span></span>

<span class="s1"><span class="Apple-converted-space"> 
</span></span>interimSubj<span class="s1"> = </span><span
class="s3">".3,.7"</span><span class="s1"><span
class="Apple-converted-space"> </span></span>

<span class="Apple-converted-space">  </span>)

\

\#\# Calls [<span class="s5">emaxfit.R</span>](emaxfit.r.html)<span
class="s4"> </span>- helper functions for fitting an emax model

\#\#<span class="Apple-converted-space">  </span>or linear model if the
emax fit does not converge

\#\# Also calculates standard error of predicted effect at each dose

\

source<span class="s1">(</span><span class="s3">"[<span
class="s5">emaxfit.R</span>](emaxfit.r.html)</span>"<span
class="s1">)</span>

\

emaxCode<span class="s1"> \<- </span><span
class="s6">function</span><span class="s1">(</span>data<span
class="s1">){</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>uniDoses<span class="s1"> \<- </span>sort<span
class="s1">( </span>unique<span class="s1">(</span>data<span
class="s1">\$</span>DOSE<span class="s1">))<span
class="Apple-converted-space"> </span></span>

<span class="s1"><span class="Apple-tab-span">
</span></span>obsMean<span class="s1"> \<- </span>tapply<span
class="s1">(</span>data<span class="s1">\$</span>RESP<span class="s1">,
</span>list<span class="s1">(</span>data<span
class="s1">\$</span>DOSE<span class="s1">), </span>mean<span
class="s1">)</span>

<span class="Apple-tab-span"> </span><span class="s4">obsSD</span> \<-
<span class="s4">tapply</span>(<span class="s4">data</span>\$<span
class="s4">RESP</span>, <span class="s4">list</span>(<span
class="s4">data</span>\$<span class="s4">DOSE</span>), <span
class="s4">sd</span>)<span class="Apple-converted-space">               
                                                   </span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>eFit<span class="s1"> \<- </span>emax.fit<span
class="s1">( </span>data<span class="s1">\$</span>RESP<span class="s1">,
</span>data<span class="s1">\$</span>DOSE<span class="s1"> )</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>outDf<span class="s1"> \<- </span>data.frame<span
class="s1">( </span>DOSE<span class="s1"> = </span>uniDoses<span
class="s1">,<span class="Apple-converted-space"> </span></span>

<span class="s1"><span class="Apple-converted-space">     
</span></span>MEAN<span class="s1"> = </span>eFit<span
class="s1">\$</span>fitpred<span class="s1">,<span
class="Apple-converted-space"> </span></span>

<span class="s1"><span class="Apple-converted-space">     
</span></span>SE<span class="s1"> = </span>eFit<span
class="s1">\$</span>sdpred<span class="s1">,</span>

<span class="s1"><span class="Apple-tab-span"> </span><span
class="Apple-converted-space">  </span></span>SDDIF<span class="s1"> =
</span>eFit<span class="s1">\$</span>sddif<span class="s1">)</span>

<span class="s1"><span class="Apple-converted-space">   
</span></span>outDf<span class="s1">\$</span>LOWER<span class="s1"> \<-
</span>outDf<span class="s1">\$</span>MEAN<span class="s1"> -
</span><span class="s2">1.96</span><span class="s1">\*</span>outDf<span
class="s1">\$</span>SE

<span class="s1"><span class="Apple-converted-space">   
</span></span>outDf<span class="s1">\$</span>UPPER<span class="s1"> \<-
</span>outDf<span class="s1">\$</span>MEAN<span class="s1"> +
</span><span class="s2">1.96</span><span class="s1">\*</span>outDf<span
class="s1">\$</span>SE

<span class="s1"><span class="Apple-converted-space">   
</span></span>outDf<span class="s1">\$</span>N<span class="s1"> <span
class="Apple-converted-space">    </span>\<- </span>table<span
class="s1">(</span>data<span class="s1">\$</span>DOSE<span
class="s1">)</span>

<span class="s1"><span class="Apple-tab-span"> </span></span>outDf<span
class="s1">\$</span>OBSMEAN<span class="s1"> \<- </span>obsMean

<span class="s1"><span class="Apple-tab-span"> </span></span>outDf<span
class="s1">\$</span>OBSSD<span class="s1"> \<- </span>obsSD

<span class="Apple-converted-space">    </span><span
class="s4">outDf</span><span class="Apple-converted-space"> </span>

}<span class="Apple-converted-space">                                   
                                                                       
       </span>

<span class="Apple-converted-space">             </span>

macroCode<span class="s1"> \<- </span><span
class="s6">function</span><span class="s1">(</span>data<span
class="s1">) {</span>

<span class="s1"><span class="Apple-converted-space">  </span></span>\#
Is effect at highest dose significant?

<span class="s1"><span class="Apple-converted-space"> 
</span></span>success<span class="s1"> \<- </span>data<span
class="s1">\$</span>LOWER<span class="s1">[</span>data<span
class="s1">\$</span>INTERIM<span class="s1">==</span>max<span
class="s1">(</span>data<span class="s1">\$</span>INTERIM<span
class="s1">) & </span>data<span class="s1">\$</span>DOSE<span
class="s1">==</span>max<span class="s1">(</span>data<span
class="s1">\$</span>DOSE<span class="s1">)] \> </span><span
class="s2">7</span>

<span class="s1"><span class="Apple-converted-space"> 
</span></span>data.frame<span class="s1">( </span>SUCCESS<span
class="s1"> = </span>success<span class="s1"> )</span>

}

<span class="Apple-converted-space">  </span>

interimCode<span class="s1"> \<- </span><span
class="s6">function</span><span class="s1">( </span>data<span
class="s1"> ){</span>

<span class="s1"><span class="Apple-converted-space">  </span></span>\#
DROP any doses where the lower bound of the difference from placebo is
negative

<span class="s1"><span class="Apple-converted-space"> 
</span></span>dropdose<span class="s1"><span
class="Apple-converted-space">  </span>\<- </span>with<span class="s1">(
</span>data<span class="s1"> , </span>DOSE<span class="s1"> [
</span>LOWER<span class="s1"> \< </span><span class="s2">0</span><span
class="s1"> & </span>DOSE<span class="s1"> != </span><span
class="s2">0</span><span class="s1">] )</span>

<span class="s1"><span class="Apple-converted-space"> 
</span></span>outList<span class="s1"> \<- </span>list<span
class="s1">()</span>

<span class="s1"><span class="Apple-converted-space"> 
</span></span><span class="s6">if</span><span class="s1">(
</span>length<span class="s1">(</span>dropdose<span class="s1">) \>
</span><span class="s2">0</span><span class="s1"> ) </span>outList<span
class="s1">\$</span>DROP<span class="s1"> \<- </span>dropdose

<span class="s1"><span class="Apple-converted-space"> 
</span></span>outList<span class="s1">\$</span>STOP<span class="s1"> \<-
</span>length<span class="s1">(</span>dropdose<span class="s1">) ==
</span>nrow<span class="s1">(</span>data<span class="s1">)-</span><span
class="s2">1</span>

<span class="s1"><span class="Apple-converted-space"> 
</span></span>outList

}

<span class="Apple-converted-space">   </span>

analyzeData<span class="s1">(</span>analysisCode<span class="s1"> =
</span>emaxCode<span class="s1">, </span>macroCode<span class="s1"> =
</span>macroCode<span class="s1">,<span
class="Apple-converted-space"> </span></span>

<span class="s1"><span class="Apple-converted-space"> 
</span></span>interimCode<span class="s1"> = </span>interimCode<span
class="s1"> )</span>

\

\

parseCovMatrix<span class="s1">(</span><span class="s2">1</span><span
class="s1">,</span><span class="s2">2</span><span class="s1">)</span>

parseCovMatrix<span class="s1">(</span>c<span class="s1">(</span><span
class="s2">1</span><span class="s1">,</span><span
class="s2">2</span><span class="s1">),</span><span
class="s2">2</span><span class="s1">)</span>

parseCovMatrix<span class="s1">(</span>c<span class="s1">(</span><span
class="s2">1</span><span class="s1">,</span><span
class="s2">0.8</span><span class="s1">,</span><span
class="s2">2</span><span class="s1">),</span><span
class="s2">2</span><span class="s1">)</span>

parseCovMatrix<span class="s1">(</span>c<span class="s1">(</span><span
class="s2">1</span><span class="s1">,</span><span
class="s2">3</span><span class="s1">,</span><span
class="s2">2</span><span class="s1">),</span><span
class="s2">2</span><span class="s1">)</span>
