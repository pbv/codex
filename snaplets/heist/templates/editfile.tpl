<apply template="base">
<h2><edit_path/></h2>
<form id="editform" method="POST"
      action="/edit/${edit_path}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor"><edit_source/></inputAceEditor></p>
<p><input type="submit" value="Gravar"/></p>
<input type="hidden" id="editform.path" value="${edit_path}"/>
</form>
<p><a href="/problems">Voltar à lista de problemas</a>
<script type="text/javascript">
startAceEditor('editform.editor');
setAceEditorModeExt('editform.editor', 'editform.path');
</script>
</apply>
