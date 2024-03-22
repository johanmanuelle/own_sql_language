En el apartado de Consultas el cual intenta recrear el concepto de tabla se implementaron las
siguientes funciones:
 si existe algun problema con esta funcionalidad
remitirse al archivo consultasOb.rkt y ejecurarlo

● table: esta función nos permite agregar una tabla
su sintaxis es table NombreTabla Atributo1 Atributo2 AtributoN…..
![image](https://github.com/johanmanuelle/own_sql_language/assets/728012/67839c4e-0b8b-4ed8-8251-ff87c653c207)

insert: Funcion que permite agregar registros a la tabla su sintaxis es: insert NombreTabla
registro1 registro2 registroN
Ejemplo:
![image](https://github.com/johanmanuelle/own_sql_language/assets/728012/764b29fc-a848-472d-b81c-0ef0fa8aa752)
es importante aclarar que si el numero de registros no es igual a los atributos, el programa no
dejara ingresar los datos, ademas de que si no se especifica un nombre de una tabla existente

remover: elimina un registro de una tabla en la base de datos, usando su llave primaria la
cual es el primer campo que ingresamos al agregar la tabla. Sintaxis: remover
NombreTabla PrimerRegistroCampo
ejemplo:
![image](https://github.com/johanmanuelle/own_sql_language/assets/728012/c2860c37-ffbb-4655-bca8-c714e393b4f1)

deltable: elimina una tabla siempre y cuando la tabla este vacía sintaxis: deltable
NombreTabla
Ejemplo:
![image](https://github.com/johanmanuelle/own_sql_language/assets/728012/dd4c39f8-409d-45ea-b4a0-4a4232681b8e)

consultaDB: me devuelve los campos que existen en una tabla. Sintaxis: consultaDB
NombreTabla
Ejemplo:
![image](https://github.com/johanmanuelle/own_sql_language/assets/728012/e2140890-3364-4df7-a0c2-17c964918de7)

gProcedure: Guarda un procedimiento relacionado una tabla para luego ser aplicado a
esta sintaxis gProcedure NombreProcedimiento parametros
EvalProcedure: Evalua el Procedimiento se ha creado y lo aplica sintaxis EvalProcedure
NombreProcCreado campoAgregar1 CampoAgregar2 CampoAgregarN
Ejemplo:
![image](https://github.com/johanmanuelle/own_sql_language/assets/728012/bcb6dd5a-bc34-4e25-b137-24acc33ff6a8)

selectAll: Nos permite Observar las tablas con sus respectivos campos
Ejemplo:
![image](https://github.com/johanmanuelle/own_sql_language/assets/728012/01935df4-a958-4b64-8a3b-c8e58b5930b1)




















