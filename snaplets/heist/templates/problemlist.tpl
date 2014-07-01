<apply template="base">
<h1>Problemas</h1>

<span>
<div class="filterlist">
<form action="/problems" method="get">
<dl>
<tagList>
<dt><tagCheckbox/></dt>
</tagList>
</dl>
</form>
</div>

<div class="problemlist">
<dl>
<problemList>
  <dt><a href="/problems/${problemID}"><problemTitle/></a> 
    <ifAccepted><img src="/icons/16x16/accepted.png" alt="Accepted"/>
    </ifAccepted>
  </dt>
  <dd class="problemli"><span class="info">
      <ifCount>
	<count/> submissões já efetuadas.<br/>
      </ifCount>
      <ifEarly>Submissões iniciam em <startTime/>.</ifEarly>
      <ifLate>Submissões terminaram em <endTime/>.</ifLate>
      <ifLimited>Submissões terminam em <endTime/>; tempo disponível: <em><timeLeft/></em>.
      </ifLimited>
  </span></dd>
</problemList>
</dl>
</div>
</span>
</apply>
