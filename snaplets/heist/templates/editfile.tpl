<apply template="base">

<h2><edit_path/></h2>
<bind tag="postAction">/admin/edit/${edit_path}</bind>
<bind tag="buttonText">Gravar</bind>
<bind tag="editMode">markdown</bind>
<apply template="_editor"><edit_source/></apply>
</apply>


