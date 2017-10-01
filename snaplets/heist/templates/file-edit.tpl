<apply template="_base">
  <h1>/<file-path/></h1>
  <p>Mime-type: <file-mime/></p>
  <if-image-file>
    <img src="${page-url}"/>
  </if-image-file>
  <if-text-file>
    <form id="editform" method="POST" action="${file-url}"
	  onSubmit="submitAceEditorText('editform.editor');">
      <input type="hidden" name="_method" value="PUT"/>
      <input type="hidden" id="editform.path" value="${file-path}"/>
      <inputAceEditor id="editform.editor" mode="ace/mode/text"><file-contents/></inputAceEditor>
      <p><input type="submit" value="Gravar alterações"/> &nbsp;
	<a class="button" title="Voltar ao diretório anterior" href="${file-parent-url}">Cancelar</a>
      </p>
    </form>
    <script type="text/javascript">
      setAceEditorModeExt('editform.editor', 'editform.path');
    </script>
  </if-text-file>

  <div>
    <form id="deleteform" method="POST" action="${file-url}"
       style="display:inline-block;">
      <input type="hidden" name="_method" value="DELETE"/>
      <input type="button" value="Apagar"
          title="Apagar ficheiro"  onClick="confirmDelete()"/>
    </form> &nbsp;
    <script type="text/javascript">
      function confirmDelete() {
      var r = confirm("Remover ficheiro (esta operação não é reversível)?");
      if (r) {
        var form = document.getElementById("deleteform");
        form.submit();
      }
      }
    </script>
  <form method="POST" action="${file-url}"
	style="display:inline;">
    <span>
      <input type="hidden" name="_method" value="PATCH"/>
      <input type="submit" value="Mudar nome:"/>
      <input type="text" name="destname" required="required"/>
    </span>
  </form>
  </div>
</apply>

<apply template="_browse">
  <li><a title="Ver página formatada" href="${page-url}">Visualizar</a></li>
</apply>
