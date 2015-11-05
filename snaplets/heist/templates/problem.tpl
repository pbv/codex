<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_overdue"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>
<apply template="base">
<apply template="_warnings"/>
<problemHeader/>
<problemDescription/>
<ifLate><p>Submissões fecharam em: <endTime/>.</ifLate>
<ifOpen>
  <ifLimited>
    <p>Tempo disponível: <remainingJsTimer/></p> 
  </ifLimited>
</ifOpen>
<ifSubmitted>
<h2>Submissões anteriores</h2>
<ol class="submissions">
<submissions>
  <li class="submissionli">
  <a href="/submissions/${problemID}/${submitID}"><span class="info"><submitStatus/><ifAccepted>&nbsp;<icon_accepted/></ifAccepted><ifRejected>&nbsp;<icon_rejected/></ifRejected><ifOverdue>&nbsp;<icon_overdue/></ifOverdue></span></a>
  </li>
</submissions>
</ol>
</ifSubmitted>

<h2>Nova submissão</h2>
<form id="editform" method="POST" 
      action="/submissions/${problemID}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor"><problemDefault/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/>  &nbsp;
   <a href="/problems" class="button">Voltar à lista de problemas</a>
</form>
<script type="text/javascript">
startAceEditor('editform.editor');
setAceEditorMode('editform.editor', 'ace/mode/python');
</script>
</apply>

