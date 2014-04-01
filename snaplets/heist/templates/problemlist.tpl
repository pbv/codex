<apply template="base">
<h1>Problemas</h1>
<dl>
<problemList>
  <dt><a href="/problems/${problemid}"><problem/></a> 
    <ifAccepted><img src="/icons/16x16/accepted.png" alt="Accepted"/>
    </ifAccepted>
  </dt>
  <dd class="problemli"><span class="info">
      <ifSubmissions>
	<numberOfSubmissions/> submissões já efetuadas.<br/>
      </ifSubmissions>
      <ifEarly>Submissões iniciam em <startTime/>.</ifEarly>
      <ifLate>Submissões terminaram em <endTime/>.</ifLate>
      <ifAcceptable>Submissões terminam em <endTime/>.<br/>
         Tempo disponível: <em><timeLeft/></em>.
      </ifAcceptable>
  </span></dd>
</problemList>
</dl>
</apply>
