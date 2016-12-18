<apply template="_base">
  <h1>/<file-path/></h1>
  <p>Mime-type: <file-mime/></p>
  <if-image-file>
    <img src="/pub/${file-path-url}"/>
  </if-image-file>
  <if-text-file>
    <form id="editform" method="POST" action="/files/${file-path-url}"
	  onSubmit="submitAceEditorText('editform.editor');">
      <input type="hidden" name="_method" value="PUT"/>
      <input type="hidden" id="editform.path" value="${file-path-url}"/>
      <inputAceEditor id="editform.editor" mode="ace/mode/text"><file-contents/></inputAceEditor>
      <p><input type="submit" value="Save changes"/> &nbsp;
	<a class="button" title="Go to parent dir" href="/files/${file-path-url}/..">Cancel</a> 
      </p>
    </form>
    <script type="text/javascript">
      setAceEditorModeExt('editform.editor', 'editform.path');
    </script>
  </if-text-file>

  <div>
    <form id="deleteform" method="POST" action="/files/${file-path-url}"
       style="display:inline-block;">
      <input type="hidden" name="_method" value="DELETE"/>
      <input type="button" value="Delete file" onClick="confirmDelete()"/>
    </form> &nbsp;
    <script type="text/javascript">
      function confirmDelete() {
      var r = confirm("Are you sure you want to delete this file?\nWARNING: this action cannot be undone.");
      if (r) {
        var form = document.getElementById("deleteform");
        form.submit();
      }
      }
    </script>
  <form method="POST" action="/files/${file-path-url}" 
	style="display:inline;">
    <span>
      <input type="hidden" name="_method" value="PATCH"/>
      <input type="submit" value="Rename file:"/> 
      <input type="text" name="destname" required="required"/>
    </span>
  </form>
  </div>
</apply>

<apply template="_browse">
  <a class="button" title="View formated page" 
     href="/pub/${file-path-url}">View</a>  
  &nbsp;
</apply>
