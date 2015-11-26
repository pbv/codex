<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>
<apply template="base">
<div class="worksheet">
  <worksheetItems>
    <ifProblem>
   <h3><a class="button" href="/docs/${documentPath}?problem=${problemID}"><problemTitle/></a><ifAccepted>&nbsp;<icon_accepted/></ifAccepted></h3>
   <div class="info">
     <ifTimed>
       <ifOpen>
	 Submissões terminam a <em><endTime/></em>; tempo disponível: <em><remainingTime/></em>.
	 <else/>
	 Submissões terminaram em <em><endTime/></em>.
       </ifOpen>
     </ifTimed>
     <ifSubmissions><submissions/> submissões efetuadas; <accepted/> aceites.</ifSubmissions>

   </div>
   <else/>
   <itemBlocks/>
 </ifProblem>
</worksheetItems>
</div>
</apply>
