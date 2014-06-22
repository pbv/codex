<apply template="base">
  <h1>Problem <problemID/> : <problemTitle/></h1>
  <bind tag="postAction">/submissions/${problemID}</bind>
  <bind tag="submitText">Enviar</bind>
  <apply template="_submission"/>
  <div id="footer">  
  <p><a href="/problems/${problemID}">Return to problem</a>    
  </div>
</apply>
