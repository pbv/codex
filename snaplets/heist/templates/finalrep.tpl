<apply template="base">
<h1>Relatório de submissão</h1>
<ul>
<li>Leia este relatório atentamente;</li>
<li><strong>Confirme que deseja sair no final desta página.</strong></li>
</ul>

<problem_list>
  <h2><problem_title/></h2>
  <p><strong>Resultado:</strong> 
  <if_accepted>Passou todos os testes.</if_accepted>
  <if_overdue>Passou todos os testes (fora do prazo).</if_overdue>
  <if_rejected>Falhou algum(s) teste(s).</if_rejected>
  <pre>
  <submit_text/>
  </pre>
  <if_rejected>
  <pre>
  <submit_report/>
  </pre>
  </if_rejected>
</problem_list>

<h1>Terminar a sessão?</h1>
<p>
<a class="button" style="font-size:125%" href="/logout">Terminar e sair</a>&nbsp;
<a class="button" style="font-size:125%" href="/problems">Voltar a trás</a>
</p>
</apply>

