<apply template="base">
<h1>Relatório de submissão</h1>
<ul>
<li>Leia este relatório atentamente;</li>
<li><strong>Confirme que deseja terminar e imprimir o relatório (no final desta página).</strong></li>
</ul>

<problemList>
  <h2><problemTitle/></h2>
  <p><strong>Resultado:</strong> 
  <ifAccepted>Passou todos os testes.</ifAccepted>
  <ifOverdue>Submetido fora do prazo.</ifOverdue>
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
<ul>
<li style="font-size:150%"><a href="/logout">Sim (imprimir e sair).</a></li>
<li style="font-size:150%"><a href="/problems">Não (voltar a trás).</a></li>
</ul>
</apply>
