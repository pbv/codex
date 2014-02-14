<apply template="base">
<h1><problem/>
   <ifAccepted><img src="/icons/24x24/accepted.png" alt="Accepted"/></ifAccepted>
</h1>
<description/>
<p>Tempo disponível: <em><timeLeft/></em></p>

<ifSubmissions>
<h2>Submissões anteriores</h2>
<ol class="submissions">
<submissions>
  <li class="submissionli">
  <a href="/submissions/${problemid}/${submitid}"><submitid/></a><span class="info"><status/></span>
</submissions>
</ol>
</ifSubmissions>

<h2>Nova submissão</h2>

<bind tag="postAction">/submissions/${problemid}</bind>
<bind tag="buttonText">Enviar</bind>
<apply template="_submission"><submitText/></apply>

<p><a href="/problems">Voltar à lista de problemas</a>
</apply>
