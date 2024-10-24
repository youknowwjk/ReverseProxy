//package com.autel.cloud.pile.base.config;
//
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//import org.springframework.web.servlet.LocaleResolver;
//import org.springframework.web.servlet.i18n.AcceptHeaderLocaleResolver;
//
//import java.util.Locale;
//
//
///**
// * @version 0.0.1-SNAPSHOT
// * @Class: LocaleConfig.java
// * @Description: 配置国际化语言
// * @Author A19011 欧志黎
// * @Date 2020年10月16日
// */
//@Configuration
//public class LocaleConfig {
//
//    /**
//     * 默认解析器 其中locale表示默认语言
//     */
//    @Bean
//    public LocaleResolver localeResolver() {
//        AcceptHeaderLocaleResolver resolver = new AcceptHeaderLocaleResolver();
//        resolver.setDefaultLocale(Locale.ENGLISH);
//        return resolver;
//    }
//}