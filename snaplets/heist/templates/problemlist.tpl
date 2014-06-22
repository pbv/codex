<apply template="base">
<h1>Problemas</h1>
<dl>
<problemList>
  <dt><a href="/problems/${problemID}"><problemTitle/></a> 
    <ifAccepted><img src="/icons/16x16/accepted.png" alt="Accepted"/>
    </ifAccepted>
  </dt>
  <dd class="problemli"><span class="info">
      <ifSubmissions>
	<countSubmissions/> submissões já efetuadas.<br/>
      </ifSubmissions>
      <ifEarly>Submissões iniciam em <startTime/>.</ifEarly>
      <ifLate>Submissões terminaram em <endTime/>.</ifLate>
      <ifLimited>Submissões terminam em <endTime/>; tempo disponível: <em><timeLeft/></em>.
      </ifLimited>
  </span></dd>
</problemList>
</dl>
</apply>
