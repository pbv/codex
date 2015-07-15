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
<form id="editform" method="POST" 
      action="/submissions/${problem_id}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor"><submit_text/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/> 
&nbsp;<a class="button" href="/problems/${problem_id}">Voltar ao problema</a>
&nbsp;<a class="button" href="/problems">Voltar à lista de problemas</a>
</form>
<script type="text/javascript">
startAceEditor('editform.editor');
setAceEditorMode('editform.editor', 'ace/mode/python');
</script>
</apply>
