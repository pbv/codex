
<apply template="_base">
  <ace-editor-js/>

  <apply template="_browse">
    <ifAdmin>
      <li><a class="icon"
	     title="Voltar ao diretório anterior"
	     href="${file-parent-url}">&curvearrowleft;</a>
    </ifAdmin>
  </apply>
 
 <h1>/<file-path/></h1>
  <p>Mime-type: <file-mime/></p>
  <if-image-file>
    <img src="${page-url}"/>
  </if-image-file>
  <if-text-file>
    <form id="editform" method="POST" action="${file-url}"
	  style="display:inline-block;">
      <input type="hidden" name="_method" value="PUT"/>
      <input type="hidden" id="editform.path" value="${file-path}"/>
      <textarea name="editform.text" id="editform.text" style="display:none;"/>
      <input type="submit" value="Gravar alterações"/> &emsp;
      <button type="button" onclick="window.open('${page-url}')">Visualizar</button> &emsp;&emsp;&emsp;&emsp;
      <apply template="_fontsize"/>
      </form>
  </if-text-file> 
      <form id="deleteform" method="POST" action="${file-url}"
	    style="display:inline-block;">
      <input type="hidden" name="_method" value="DELETE"/> &emsp;
      <input type="button" value="Apagar"
             title="Apagar ficheiro"  onClick="confirmDelete()"/>
      </form>
  <form method="POST" action="${file-url}"
	style="display:inline;">
    <span>
      <input type="hidden" name="_method" value="PATCH"/>
      <input type="submit" value="Mudar nome:"/>
      <input type="text" name="destname" required="required"/>
    </span>
  </form>
      <if-text-file>
	<div id="editor"><file-contents/></div>
      </if-text-file>
    <script type="text/javascript">
      var editor = startAceEditor('editor');
      setAceEditorModeExt(editor, document.getElementById('editform.path').value);
      document.getElementById('editform').addEventListener('submit',
      function() { submitListener(editor, 'editform.text'); });
    </script>

    <script type="text/javascript">
      function confirmDelete() {
      var r = confirm("Remover o ficheiro (esta operação não é reversível)?");
      if (r) {
        var form = document.getElementById("deleteform");
        form.submit();
      }
      }
    </script>

</apply>

