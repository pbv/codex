<apply template="base">
  <ifLoggedIn>
     <p>Está autenticado como <strong><loggedInUser/></strong></p>
     <p>Pode consultar a <a href="/problems">lista de problemas</a> disponíveis.</p>
  </ifLoggedIn>

  <ifLoggedOut>
  <apply template="_login"/> 
  </ifLoggedOut>

</apply>
