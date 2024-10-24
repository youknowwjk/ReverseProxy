//package com.autel.cloud.pile.base.config;
//
//import org.springframework.beans.factory.annotation.Configurable;
//import org.springframework.util.StringUtils;
//import org.springframework.web.servlet.LocaleResolver;
//
//import javax.servlet.http.HttpServletRequest;
//import javax.servlet.http.HttpServletResponse;
//import java.util.Locale;
//
///**
// * @ClassName InternationalLocalResolver
// * @Author A22121
// * @Description
// * @Date 2022/4/29 15:20
// * @Version 0.0.1-SNAPSHOT
// */
//@Configurable
//class InternationalLocaleResolver implements LocaleResolver {
//    @Override
//    public Locale resolveLocale(HttpServletRequest httpServletRequest) {
//        String language = httpServletRequest.getHeader("accept-language");
//        Locale local = Locale.getDefault();
//        if (StringUtils.hasText(language)){
//            String[] s = language.split("_");
//            local = new Locale(s[0],s[1]);
//        }
//        return local;
//    }
//
//    @Override
//    public void setLocale(HttpServletRequest request, HttpServletResponse response, Locale locale) {
//
//    }
//}
