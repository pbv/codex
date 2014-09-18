<apply template="base">
<h1><problemTitle/><ifAccepted
  ><img src="/icons/24x24/accepted.png" alt="Accepted"/></ifAccepted>
</h1>
<description/>
<ifOpen>
  <ifLimited>
    <p>Tempo disponível: <em><timeLeft/></em></p> 
  </ifLimited>
  <else>
    <ifEarly>Submissões vão abrir em: <startTime/>.</ifEarly>
    <ifLate>Submissões fecharam em: <endTime/>.</ifLate>
  </else>
</ifOpen>

<ifSubmitted>
<h2>Submissões anteriores</h2>
<ol class="submissions">
<submissions>
  <li class="submissionli">
  <a href="/submissions/${problemID}/${submitID}"><span class="info"><submitStatus/></span></a>
  </li>
</submissions>
</ol>
</ifSubmitted>

<ifEarly>
<else>
<h2>Nova submissão</h2>
<bind tag="postAction">/submissions/${problemID}</bind>
<bind tag="buttonText">Enviar</bind>
<apply template="_submission"><submitText/></apply>
</else>
</ifEarly>
<p><a href="/problems">Voltar à lista de problemas</a>
</apply>
