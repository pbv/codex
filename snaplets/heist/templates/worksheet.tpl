<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>
<apply template="base">
<div class="worksheet">
  <worksheetItems>
    <ifProblem>
   <h3><a class="button" href="/${documentPath}?problem=${problemID}"><problemTitle/></a><ifAccepted>&nbsp;<icon_accepted/></ifAccepted></h3>
   <div class="info">
     <ifTimed>
       <ifOpen>
	 Submissions will end on <em><endTime/></em>; remaining time: <em><remainingTime/></em>.
	 <else/>
	 Submissions ended on <em><endTime/></em>.
       </ifOpen>
     </ifTimed>
     <ifSubmissions><submissions/> submissions; <accepted/> accepted.</ifSubmissions>
   </div>
   <else/>
   <itemBlocks/>
 </ifProblem>
</worksheetItems>
</div>
</apply>
