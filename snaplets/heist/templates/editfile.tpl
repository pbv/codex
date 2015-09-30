<apply template="base">
<apply template="_warnings"/>
<h2><edit_path/></h2>
<form id="editform" method="POST"
      action="/edit/${edit_path}?pid=${problem_id}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor"><edit_source/></inputAceEditor></p>
<p><input type="submit" value="Gravar"/>
&nbsp; <input type="button" value="Cancelar" onclick="window.history.back();"/>
<input type="hidden" id="editform.path" value="${edit_path}"/>
</p>
</form>

<script type="text/javascript">
startAceEditor('editform.editor');
setAceEditorModeExt('editform.editor', 'editform.path');
</script>
</apply>
