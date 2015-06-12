<bind tag="accept_icon"><img src="/icons/16x16/accepted.png" alt="Accepted"/></bind>
<apply template="base">
<problem_description/>
<if_early><p>Submissões vão abrir em: <start_time/>.</if_early>
 <if_late><p>Submissões fecharam em: <end_time/>.</if_late>


<if_open>
  <if_limited>
    <p>Tempo disponível: <em><remaining_time/></em></p> 
  </if_limited>
</if_open>

<if_open>
<if_limited>
<h3>Tempo disponível: <remaining_js_timer/></h3>
</if_limited>
</if_open>


<if_submitted>
<h2>Submissões anteriores</h2>
<ol class="submissions">
<submissions>
  <li class="submissionli">
  <a href="/submissions/${problem_id}/${submit_id}"><span class="info"><submit_status/><if_accepted>&nbsp;<accept_icon/></if_accepted></span></a>
  </li>
</submissions>
</ol>
</if_submitted>


<h2>Nova submissão</h2>
<bind tag="postAction">/submissions/${problem_id}</bind>
<bind tag="buttonText">Enviar</bind>
<apply template="_submission"><problem_default/></apply>
<p><a href="/problems">Voltar à lista de problemas</a>
</apply>
