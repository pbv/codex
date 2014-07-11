<apply template="base">
<h1>Problemas</h1>
<div class="filterlist">
<form action="/problems" method="get">
<dl>
<dt>Filtrar problemas:</dt>
<dt class="info">(<visibleProblems/> de <totalProblems/> vísiveis)</dt>
<tagList>
<dt><tagCheckbox/></dt>
</tagList>
</dl>
<p><a href="/problems">Limpar filtros</a></p>
</form>
</div>

<div class="problemlist">
<dl>
<problemList>
  <dt><a href="/problems/${problemID}"><problemTitle/></a><ifAccepted><img src="/icons/16x16/accepted.png" alt="Accepted"/></ifAccepted>
  </dt>
  <dd class="problemli"><span class="info">
      <ifSubmitted>
	<count/> submissões efetuadas.<br/>
      </ifSubmitted>
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
