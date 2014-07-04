<apply template="base">
<h1>Problemas</h1>


<div class="filterlist">
<form action="/problems" method="get">
Filtros:
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
      <ifAcceptable>
      <ifLimited>Submissões terminam em <endTime/>; tempo disponível: <em><timeLeft/></em>.
      </ifLimited>
	<else>
	  <ifEarly>Submissões vão abrir em: <startTime/>.</ifEarly>
	  <ifLate>Submissões fecharam em: <endTime/>.</ifLate>
	</else>
      </ifAcceptable>
  </span></dd>
</problemList>
</dl>
</div>

</apply>
