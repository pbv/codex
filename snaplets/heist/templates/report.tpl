<apply template="base">
<h1><pageTitle/></h1>
<h2>Submissão <submitID/></h2>
<ifEarly>
  <p>Submissão antecipada; o resultado será visível depois de <validFrom/>.</p>
<else/>
  <h2>Resultado: <em><submitClassify/></em></h2>
  <pre>
    <submitMessage/>
  </pre>
</ifEarly>

<h2>Nova submissão</h2>
<form id="editform" method="POST" 
      action=""
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor" mode="ace/mode/${pageLanguage}"><submitCodeText/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/> 
&nbsp;<a class="button" href="${pagePath}">Voltar ao problema</a>
&nbsp;<a class="button" href="">Voltar à folha de problemas</a>
</form>
</apply>
