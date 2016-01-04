<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_overdue"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>
<apply template="base">
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
<input class="toggle-box" id="header1" type="checkbox" >
<label for="header1">Submissões anteriores</label> 
<div class="submissions">
<ol>
<submissionList>
  <li class="submissionli"><a href="/${documentPath}?problem=${problemID}&submit=${submitID}"><submitResult/><ifOverdue>&nbsp;<icon_overdue/></ifOverdue></a>
  </li>
</submissionList>
</ol>
</div>
</ifSubmissions>

<h2>Nova submissão</h2>
<form id="editform" method="POST" 
      action="/${documentPath}?problem=${problemID}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id='editform.editor' mode='ace/mode/${problemLanguage}'><problemSubmit/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/>  &nbsp;
   <a href="/${documentPath}" class="button">Voltar à folha de problemas</a>
</form>
</apply>

