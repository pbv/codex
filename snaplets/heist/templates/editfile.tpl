<apply template="base">
<apply template="_warnings"/>
<h1><edit-path/></h1>
<form id="editform" method="POST"
      action="/edit/${edit-path}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor" mode="ace/mode/text"><edit-text/></inputAceEditor></p>
<p><input type="submit" value="Gravar"/>
&nbsp; <input type="button" value="Cancelar" onclick="window.history.back();"/>
<input type="hidden" id="editform.path" value="${edit-path}"/>
</p>
</form>

<script type="text/javascript">
setAceEditorModeExt('editform.editor', 'editform.path');
</script>
</apply>
