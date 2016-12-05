<apply template="base">
<h1>/<file-path/></h1>
<form id="editform"  method="POST" action="/edit/${file-path}"
      onsubmit="submitAceEditorText('editform.editor');">
  <input type="hidden" id="editform.path" value="${file-path}"/>
  <inputAceEditor id="editform.editor" 
     mode="ace/mode/text"><file-contents/></inputAceEditor>
  <p><input type="submit" value="Save"/>  &nbsp; 
    <a class="button" href="/browse/${file-dir}">Cancel</a>
  </p>
</form>

<script type="text/javascript">
setAceEditorModeExt('editform.editor', 'editform.path');
</script>
</apply>
