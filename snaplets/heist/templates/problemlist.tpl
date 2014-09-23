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
<p><input type="submit" value="Aplicar"/></p>
<p><a href="/problems">Limpar filtros</a></p>
</form>
</div>

<div class="problemlist">
<dl>
<problemList>
  <ifOpen><dt><a href="/problems/${problemID}"><problemTitle/></a><ifAccepted><img src="/icons/16x16/accepted.png" alt="Accepted"/></ifAccepted></dt>
    <dd class="problemli"><span class="info">
	<ifSubmitted><count/> submissões já efetuadas.<br/></ifSubmitted>
	<ifLimited>Submissões terminam em <endTime/>; tempo disponível: <em><timeLeft/></em>.
      </ifLimited>
  </span></dd>
    <else><dt><problemTitle/><dt/>
	<dd>
	<ifEarly><span class="info">Submissões iniciam em <startTime/>.</span></ifEarly>
	<ifLate><span class="info">Submissões fecharam em <endTime/>.</span></ifLate>
	</dd>
    </else>
  </ifOpen>
</problemList>
</dl>
</div>

</apply>
