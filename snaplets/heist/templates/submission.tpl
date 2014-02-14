<apply template="base">
  <h1>Problem <problemid/> : <problemtitle/></h1>
  <bind tag="postAction">/submissions/${problemid}</bind>
  <bind tag="submitText">Enviar</bind>
  <apply template="_submission"/>
  <div id="footer">  
  <p><a href="/problems/${problemid}">Return to problem</a>    
  </div>
</apply>
