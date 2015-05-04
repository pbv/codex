<apply template="base">
<ifAccepted><span class="note"><img src="/icons/24x24/accepted.png" alt="Accepted"/></span>
</ifAccepted>
<probDoc/>
<ifOpen>
  <ifLimited>
    <p>Tempo disponível: <em><timerLeft/></em></p> 
  </ifLimited>
  <else>
    <ifEarly><p>Submissões vão abrir em: <timerStart/>.</ifEarly>
    <ifLate><p>Submissões fecharam em: <timerEnd/>.</ifLate>
  </else>
</ifOpen>

<ifSubmitted>
<h2>Submissões anteriores</h2>
<ol class="submissions">
<submissions>
  <li class="submissionli">
  <a href="/submissions/${probID}/${submitID}"><span class="info"><submitStatus/></span></a>
  </li>
</submissions>
</ol>
</ifSubmitted>

<ifEarly>
<else>
<h2>Nova submissão</h2>
<bind tag="postAction">/submissions/${probID}</bind>
<bind tag="buttonText">Enviar</bind>
<apply template="_submission"><probDefault/></apply>
</else>
</ifEarly>
<p><a href="/problems">Voltar à lista de problemas</a>
</apply>
