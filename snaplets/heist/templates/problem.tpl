<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_overdue"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>
<apply template="base">
<apply template="_warnings"/>
<problemHeader/>
<problemDescription/>
<ifTimed>
  <ifOpen>
    <p>Submissões terminam a <em><endTime/></em>; 
      tempo disponível: <em><remainingTime/></em></p> 
    <else/>
    <p>Submissões terminaram em <em><endTime/></em>.
  </ifOpen>
</ifTimed>
<ifSubmissions>
<h2>Submissões anteriores</h2>
<ol class="submissions">
<submissionList>
  <li class="submissionli">
  <a href="/docs/${documentPath}?problem=${problemID}&submit=${submitID}"><span class="info"><submitStatus/><ifAccepted>&nbsp;<icon_accepted/></ifAccepted><ifRejected>&nbsp;<icon_rejected/></ifRejected><ifOverdue>&nbsp;<icon_overdue/></ifOverdue></span></a>
  </li>
</submissionList>
</ol>
</ifSubmissions>

<h2>Nova submissão</h2>
<form id="editform" method="POST" 
      action="/docs/${documentPath}?problem=${problemID}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor"><problemCode/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/>  &nbsp;
   <a href="/docs/${documentPath}" class="button">Voltar à folha de problemas</a>
</form>
<script type="text/javascript">
startAceEditor('editform.editor');
setAceEditorMode('editform.editor', 'ace/mode/python');
</script>
</apply>

