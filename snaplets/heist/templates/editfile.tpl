<apply template="base">
<apply template="_warnings"/>
<h2><editPath/></h2>
<form id="editform" method="POST"
      action="/edit/${editPath}?pid=${problemID}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor"><editText/></inputAceEditor></p>
<p><input type="submit" value="Gravar"/>
&nbsp; <input type="button" value="Cancelar" onclick="window.history.back();"/>
<input type="hidden" id="editform.path" value="${editPath}"/>
</p>
</form>

<script type="text/javascript">
startAceEditor('editform.editor');
setAceEditorModeExt('editform.editor', 'editform.path');
</script>
</apply>
