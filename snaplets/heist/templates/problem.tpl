<bind tag="accept_icon"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="reject_icon"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="overdue_icon"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="editor_icon"><img src="/icons/16x16/editor.png"/></bind>
<apply template="base">
<apply template="_warnings"/>
<problem_description/>
<if_early><p>Submissões vão abrir em: <start_time/>.</if_early>
<if_late><p>Submissões fecharam em: <end_time/>.</if_late>
<if_open>
  <if_limited>
    <p>Tempo disponível: <remaining_js_timer/></p> 
  </if_limited>
</if_open>
<ifAdmin>
  <p><a class="button" href="/edit/${problem_path}?pid=${problem_id}"><editor_icon/>&nbsp;Editar problema</a>
  <a class="button" href="/edit/${problem_doctest}?pid=${problem_id}"><editor_icon/>&nbsp;Editar testes</a>
</p></ifAdmin>

<if_submitted>
<h2>Submissões anteriores</h2>
<ol class="submissions">
<submissions>
  <li class="submissionli">
  <a href="/submissions/${problem_id}/${submit_id}"><span class="info"><submit_status/><if_accepted>&nbsp;<accept_icon/></if_accepted><if_rejected>&nbsp;<reject_icon/></if_rejected><if_overdue>&nbsp;<overdue_icon/></if_overdue></span></a>
  </li>
</submissions>
</ol>
</if_submitted>

<h2>Nova submissão</h2>
<form id="editform" method="POST" 
      action="/submissions/${problem_id}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor"><problem_default/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/>  &nbsp;
   <a href="/problems" class="button">Voltar à lista de problemas</a>
</form>
<script type="text/javascript">
startAceEditor('editform.editor');
setAceEditorMode('editform.editor', 'ace/mode/python');
</script>
</apply>

