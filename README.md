## 关于这项服务的使用说明 ##  
这是一个获取SAP的ALV报表和底表数据的HTTP接口。  
  
它以 ICF 服务的形式提供此接口，在此系统中，此服务已分配给 ICF 服务 [/sap/bc/zsicf_bi_data](http://sapdev.wiskind.com:50001/sap/bc/zsicf_bi_data "这是调用地址")。  
接口仅支持POST或者GET方法，Params仅允许传递键**tcode**以及其值“**报表事务码**”来获取SAP报表数据或者传递键**tabname**以及其值“**底表名称**”来获取SAP底表数据。  
  
获取报表数据时可通过以raw格式的JSON数据来传递Body，其内容为所获取报表的选择屏幕参数以及值，具体请查看下面的详细说明。  
请求结果以JSON格式返回  

    {
		"rtype": "",
		"rtmsg": "",
		"data": []
	}

**rtype** ：消息类型: S 成功,E 错误,W 警告,I 信息,A 中断  
**rtmsg** ：消息文本  
**data** ：数据结果  
 - 获取ALV报表数据  
以**cogi**报表为例，请求地址为：  
[http://sapdev.wiskind.com:50001/sap/bc/zsicf_bi_data?tcode=cogi](http://sapdev.wiskind.com:50001/sap/bc/zsicf_bi_data?tcode=cogi "cogi")  

	 **ZTBICONFIG**：
配置底表对应报表的选择屏幕参数名以及默认值、选择屏幕参数名映射值 

	- TCODE：报表事务码  
	- SELNAME：报表选择屏幕字段
	- ZINDEX：多选字段可配置，从1往后升序编号即可，暂定用于报表配置默认值用
	- KIND：ABAP/4: 选择类型，单选字段配置'P'，多选字段配置'S'.
	- SIGN：ABAP/4: ID: I/E (包括/不包括值)
	- ZOPTION：ABAP/4: 选择选项 (EQ/BT/CP/...)
	- LOW：默认值下限
	- HIGH：默认值上限，仅限多选字段可用
	- LOW_MAP：选择下限映射
	- HIGH_MAP:选择上限映射

	| TCODE | SELNAME  | ZINDEX | KIND | SIGN | ZOPTION | LOW     | HIGH      | LOW_MAP | HIGH_MAP  |
	| ----- | -------- | ------ | ---- | ---- | ------- | ------- | --------- | ------- | --------- |
	| COGI  | S_WERKS  | 0      | S    | I    | BT      |         | WERKS_LOW |         | WERKS_HIGH|

	**Body**：
	
		{
			"werks_low": "2000",
			"werks_high": "2010"
		}

	 **返回结果**：
	  
		{
			"rtype": "S",
			"rtmsg": "成功获取COGI的数据",
			"data": [
				{
					"box": false,
					"vbkz": "",
					"status": "@EB@",
					"weblnr": "0000001248",
					"weblpos": 1,
					"matnr": "E0210016481",
					"maktx": "xxx_xx_1000mm_0.44mm_xxx",
					"werks": "2000",
					"lgort": "B012",
					"charg": "2401310003",
					"bwtar": "",
					"bwart": "101",
					"sobkz": "",
					"erfmg": 7.4,
					"erfme": "",
					"aufnr": "001000000xxx",
					"autyp": 10,
					"msgid": "M7",
					"msgno": "022",
					"msgv1": "采购订单订货数量",
					"msgv2": "7.180",
					"msgv3": "TO",
					"msgv4": ": E0210016481 2000 B012 2401310003",
					"msgtext": "采购订单订货数量 超过了7.180 TO : E0210016481 2000 B012 2401310003",
					"budat": "2024-01-31",
					"ernam": "XXX",
					"ersda": "2024-01-31",
					"erzet": "14:32:24",
					"fwdat": "2024-01-31",
					"fwzet": "14:32:24",
					"kdauf": "",
					"kdpos": 0,
					"psPspPnr": 0,
					"fevor": "Z01",
					"dispo": "Z01",
					"blcount": 1,
					"CwmErfmg": 0,
					"CwmErfme": ""
				}
			]
		}
	  
- 获取底表数据  
以**ZTBICONFIG**底表为例，请求地址为：
[http://sapdev.wiskind.com:50001/sap/bc/zsicf_bi_data?tabname=ztbiconfig](http://sapdev.wiskind.com:50001/sap/bc/zsicf_bi_data?tabname=ztbiconfig "ztbiconfig")

	**ZTBICONFIG_TAB**：
配置底表对应底表的名称以及查询条件 
	- TABNAME：底表名称
	- WHERESTR：OPEN SQL查询条件

	| TABNAME       | WHERESTR |
	| ------------- | -------- |
	| ZTBICONFIG    |          |

	 **返回结果**：
  

		{
			"rtype": "S",
			"rtmsg": "成功获取ZTBICONFIG的数据",
			"data": [
				{
					"tcode": "COGI",
					"selname": "S_WERKS",
					"zindex": 0,
					"kind": "S",
					"sign": "I",
					"zoption": "BT",
					"low": "",
					"high": "",
					"lowMap": "WERKS_LOW",
					"highMap": "WERKS_HIGH"
				}
			]
		}

## 后续计划 ##  
1. 可能会添加权限控制，不过现有的配置表也是一种权限控制了  
2. 想得到再说吧  
  
## 提醒一下 ##  
- 添加新数据源前一定要充分测试，获取报表数据可能会有意想不到的结果  
- 有好的建议、想法欢迎指正  
  
## 联系我 ##  
邮箱：[weikj@foxmail.com](mailto:weikj@foxmail.com "kkw")
