package com.autel.cloud.pile.base.infrastructure.util;

import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.core.toolkit.StringPool;
import com.baomidou.mybatisplus.generator.AutoGenerator;
import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.rules.DateType;
import com.baomidou.mybatisplus.generator.config.rules.NamingStrategy;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

/**
 * @ClassName CodeGeneration
 * @Author A22121
 * @Description
 * @Date 2022/4/11 20:46
 * @Version 0.0.1-SNAPSHOT
 */
public class CodeGeneration {
    private static final String URL = "jdbc:mysql://10.240.3.139:3306/energy_pile_base?useLegacyDatetimeCode=" +
            "false&serverTimezone=Asia/Shanghai&useSSL=false&characterEncoding=utf8";
    private static final String USERNAME = "cloud";
    private static final String PASSWORD = "cloud";
    private static final String PROJECT_PATH = "D:\\code\\pile-base";
    private static final String SRC_PATH = "/src/main/java/";


    public static void main(String[] args) {
        // 1、创建代码生成器
        AutoGenerator mpg = new AutoGenerator();
        // 2、全局配置
        mpg.setGlobalConfig(getGlobalConfig());
        // 3、数据源配置
        mpg.setDataSource(getDataSourceConfig());
        // 4、包配置
        mpg.setPackageInfo(getPackageConfig());
        // 5、策略配置
        setStrategyConfig(mpg, "op_pile_v2g_params_setting");
        // 6、执行
        mpg.execute();
    }

    private static GlobalConfig getGlobalConfig() {
        GlobalConfig gc = new GlobalConfig();
        gc.setOutputDir(PROJECT_PATH + SRC_PATH);

        gc.setAuthor("A22121");
        gc.setOpen(false); //生成后是否打开资源管理器
        gc.setFileOverride(false); //重新生成时文件是否覆盖

        //UserServie
        gc.setServiceName("%sRepository");    //去掉Service接口的首字母I
        gc.setServiceImplName("%sRepositoryImpl");
        gc.setEntityName("%sEntity");

        gc.setIdType(IdType.ASSIGN_ID); //主键策略
        gc.setDateType(DateType.ONLY_DATE);//定义生成的实体类中日期类型
        gc.setSwagger2(true);//开启Swagger2模式
        return gc;
    }


    private static DataSourceConfig getDataSourceConfig() {
        DataSourceConfig dsc = new DataSourceConfig();
        dsc.setUrl(URL);
        dsc.setDriverName("com.mysql.cj.jdbc.Driver");
        dsc.setUsername(USERNAME);
        dsc.setPassword(PASSWORD);
        dsc.setDbType(DbType.MYSQL);
        return dsc;
    }

    private static PackageConfig getPackageConfig() {
        PackageConfig pc = new PackageConfig();
        //包 com.autel.cloud.app
        pc.setParent("com.autel.cloud.app");
        //包  com.autel.cloud.app.pile.base
        pc.setModuleName("pile.base"); //模块名
        //包 com.autel.cloud.app.pile.base.controller
        pc.setController("controller");
        pc.setEntity("infrastructure.model.entity");
        pc.setService("repository");
        pc.setServiceImpl("repository.impl");
        pc.setMapper("infrastructure.mapper");


        Map<String, String> packageInfo = new HashMap<>();
        packageInfo.put(ConstVal.CONTROLLER, pc.getParent() + ".controller");
        packageInfo.put(ConstVal.SERVICE, pc.getParent() + ".repository");
        packageInfo.put(ConstVal.SERVICE_IMPL, pc.getParent() + ".repository.impl");
        packageInfo.put(ConstVal.ENTITY, pc.getParent() + ".infrastructure.model.entity");
        packageInfo.put(ConstVal.MAPPER, pc.getParent() + ".infrastructure.mapper");

        Map<String, String> pathInfo = new HashMap<>();
        pathInfo.put(ConstVal.CONTROLLER_PATH, PROJECT_PATH + SRC_PATH + packageInfo.get(ConstVal.CONTROLLER).replaceAll("\\.", StringPool.BACK_SLASH + File.separator));
        pathInfo.put(ConstVal.SERVICE_PATH, PROJECT_PATH + SRC_PATH + packageInfo.get(ConstVal.SERVICE).replaceAll("\\.", StringPool.BACK_SLASH + File.separator));
        pathInfo.put(ConstVal.SERVICE_IMPL_PATH, PROJECT_PATH + SRC_PATH + packageInfo.get(ConstVal.SERVICE_IMPL).replaceAll("\\.", StringPool.BACK_SLASH + File.separator));
        pathInfo.put(ConstVal.ENTITY_PATH, PROJECT_PATH + SRC_PATH + packageInfo.get(ConstVal.ENTITY).replaceAll("\\.", StringPool.BACK_SLASH + File.separator));
        pathInfo.put(ConstVal.MAPPER_PATH, PROJECT_PATH + SRC_PATH + packageInfo.get(ConstVal.MAPPER).replaceAll("\\.", StringPool.BACK_SLASH + File.separator));
        pathInfo.put(ConstVal.XML_PATH, PROJECT_PATH + "\\src\\main\\resources\\mapper\\");
        pc.setPathInfo(pathInfo);
        return pc;
    }

    private static void setStrategyConfig(AutoGenerator mpg, String... tableName) {
        StrategyConfig strategy = new StrategyConfig();
        strategy.setInclude(tableName);
        strategy.setNaming(NamingStrategy.underline_to_camel);//数据库表映射到实体的命名策略
        strategy.setTablePrefix(mpg.getPackageInfo().getModuleName() + "_"); //生成实体时去掉表前缀
        strategy.setColumnNaming(NamingStrategy.underline_to_camel);//数据库表字段映射到实体的命名策略
        strategy.setEntityLombokModel(true); // lombok 模型 @Accessors(chain = true) setter链式操作
        strategy.setRestControllerStyle(true); //restful api风格控制器
        strategy.setControllerMappingHyphenStyle(true); //url中驼峰转连字符
        strategy.setEntityTableFieldAnnotationEnable(Boolean.TRUE);
        mpg.setStrategy(strategy);
    }
}
