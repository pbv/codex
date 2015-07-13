<apply template="base">
<h1><problem_title/></h1>
<h2>Submissão <submit_id/></h2>
<h2>Resultado: <em><submit_status/></em></h2>

<if_accepted>
  <p>Parabens! A sua submissão passou todos os testes.</p>
</if_accepted>
<if_overdue>
  <p>A sua submissão passou todos os testes, mas foi enviada
  fora do tempo.
</if_overdue>
<if_rejected>
  <p>A submissão foi <strong>rejeitada</strong>; o relatório  
    seguinte descreve o erro encontrado.</p>
  <pre><submit_report/></pre>
</if_rejected>

<h2>Nova submissão</h2>
<form id="codeform" method="POST" 
      action="/submissions/${problem_id}"
      onsubmit="submitAceEditorText('codeform.editor');">
<p><inputAceEditor id="codeform.editor"><submit_text/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/></p>
</form>
<p><a href="/problems">Voltar à lista de problemas</a>
<script type="text/javascript">
startAceEditor('codeform.editor');
setAceEditorMode('codeform.editor', 'ace/mode/python');
</script>
</apply>
