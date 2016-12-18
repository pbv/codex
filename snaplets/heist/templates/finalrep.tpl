<apply template="_base">
<h1>Relatório de submissão</h1>
<ul>
<li>Leia este relatório atentamente;</li>
<li><strong>Confirme que deseja sair no final desta página.</strong></li>
</ul>

<problemList>
  <h2><problemTitle/></h2>
  <p><strong>Resultado:</strong> 
  <ifAccepted>Passou todos os testes.</ifAccepted>
  <ifOverdue>Passou todos os testes (fora do prazo).</ifOverdue>
  <ifRejected>Falhou algum(s) teste(s).</ifRejected>
  <pre>
  <submitText/>
  </pre>
  <ifRejected>
  <pre>
  <submitReport/>
  </pre>
  </ifRejected>
</problemList>

<h1>Terminar a sessão?</h1>
<p>
<a class="button" style="font-size:125%" href="/logout">Terminar e sair</a>&nbsp;
<a class="button" style="font-size:125%" href="/problems">Voltar a trás</a>
</p>
</apply>

