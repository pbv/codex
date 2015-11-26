<apply template="base">
<apply template="_warnings"/>
<h1><problemTitle/></h1>
<h2>Submissão <submitID/></h2>
<h2>Resultado: <em><submitStatus/></em></h2>

<ifAccepted>
  <p>Parabens! A sua submissão passou todos os testes.</p>
</ifAccepted>
<ifOverdue>
  <p>A sua submissão passou todos os testes, mas foi enviada
  fora do tempo.
</ifOverdue>
<ifRejected>
  <p>A submissão foi <strong>rejeitada</strong>; o relatório  
    seguinte descreve o erro encontrado.</p>
  <pre><submitReport/></pre>
</ifRejected>

<h2>Nova submissão</h2>
<form id="editform" method="POST" 
      action="/docs/${documentPath}?problem=${problemID}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor"><submitCode/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/> 
&nbsp;<a class="button" href="/docs/${documentPath}?problem=${problemID}">Voltar ao problema</a>
&nbsp;<a class="button" href="/docs/${documentPath}">Voltar à folha de problemas</a>
</form>
<script type="text/javascript">
startAceEditor('editform.editor');
setAceEditorMode('editform.editor', 'ace/mode/python');
</script>
</apply>
