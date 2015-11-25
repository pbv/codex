<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>
<apply template="base">
<div class="worksheet">
<worksheetItems>
 <ifProblem>
   <h3><a href="/docs/${documentPath}?problem=${problemID}"><problemTitle/></a></h3>
   <ifSubmitted><totalSubmissions/> submissões já efetuadas.<br/></ifSubmitted>
   <ifLate>Submissões fecharam em <endTime/>.</ifLate>
   <ifOpen>
     <ifLimited>
       Submissões terminam em <endTime/>; tempo disponível: <em><remainingJsTimer/></em>.
     </ifLimited>
   </ifOpen>
   <else/>
   <itemBlocks/>
 </ifProblem>
</worksheetItems>
</div>
</apply>
