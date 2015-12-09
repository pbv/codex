<apply template="base">
<apply template="_warnings"/>
<h1><problemTitle/></h1>
<h2>Submissão <submitID/></h2>
<h2>Resultado: <em><submitResult/></em></h2>

<ifAccepted>
<ifOverdue>
  <p>A sua submissão passou todos os testes, mas foi enviada
    fora do tempo.</p>
<else/>
  <p>Parabéns! A sua submissão passou todos os testes.</p>
</ifOverdue>
<else/>
  <p>A submissão foi <strong>rejeitada</strong>.</p>
</ifAccepted>
<pre><submitMsg/></pre>

<h2>Nova submissão</h2>
<form id="editform" method="POST" 
      action="/${documentPath}?problem=${problemID}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor" mode="ace/mode/${problemLanguage}"><submitCode/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/> 
&nbsp;<a class="button" href="/${documentPath}?problem=${problemID}">Voltar ao problema</a>
&nbsp;<a class="button" href="/${documentPath}">Voltar à folha de problemas</a>
</form>
</apply>
