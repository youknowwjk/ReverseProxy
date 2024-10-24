package com.autel.cloud.pile.base.domain.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.Locale;
import java.util.Objects;

/**
 * @Function 国际化配置工具类
 * @Author A22599 MingLong
 * @Date 2022.08.26
 */
@Component("energyCloudPileBaseMessageSourceUtil")
@Slf4j
public class MessageSourceUtil {

    @Autowired
    private MessageSource messageSource;

    /**
     * @param code
     * @param language
     * @return
     * @function 根据登录用户的语言和错误码读取配置文件并返回国际化配置信息
     */
    public String getMessage(String code, String language) {
        Locale locale = null;
        try {
            language = StringUtils.isBlank(language) ? "en-US" : language;
            String[] lan = language.split("-");
            if (lan.length > 1) {
                locale = new Locale(lan[0], lan[1]);
            } else {
                locale = new Locale(lan[0]);
            }
            return messageSource.getMessage(code, null, locale);
        } catch (Exception e) {
            log.error("解析资源文件异常：{}", e.getMessage());
        }
        return "";
    }

    /**
     * @return
     * @function 获得登录用户的语言
     */
    public String getLoginUserLanguage() {
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String localeLanguage = request.getLocale().getLanguage();
        String language = request.getHeader("accept-language");
        log.info("========== the language in the request header, the localeLanguage:{}， language:{}", localeLanguage, language);
        return language;
    }
}
