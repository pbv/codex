<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_overdue"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>
<apply template="base">
<div class="description">
<pageDescription/>
</div>

<h2>Nova submissão</h2>
<form id="editform" method="POST" 
      action=""
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id='editform.editor' mode='ace/mode/${problemLanguage}'><problemCodeText/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/>  &nbsp;
   <a href="" class="button">Voltar à folha de problemas</a>
</form>
</apply>

