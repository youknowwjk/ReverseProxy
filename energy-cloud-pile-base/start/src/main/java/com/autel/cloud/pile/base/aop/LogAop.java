package com.autel.cloud.pile.base.aop;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.data.domain.R;
import com.autel.cloud.base.data.domain.pagination.IPageVO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.common.Constants;
import com.autel.cloud.common.CustomizedDateTime;
import com.autel.cloud.common.DateUtil;
import com.autel.cloud.common.FieldPattern;
import com.autel.cloud.pile.bill.dto.pay.PageResponseDTO;
import com.autel.cloud.pile.bill.vo.PageForBillVO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.vo.UserDateTimePattern;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.core.io.InputStreamSource;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


//@Component
//@Aspect
//@ComponentScan
public class LogAop {

    private static final Logger LOGGER = LoggerFactory.getLogger(LogAop.class);

    private final PileUserFeign pileUserFeign;

    public LogAop(PileUserFeign pileUserFeign) {
        this.pileUserFeign = pileUserFeign;
    }
    /**
     * 定义切点
     */
    @Pointcut("execution(* com.autel.cloud.pile.base.controller.*.*(..)) " +
            "|| execution(* com.autel.cloud.pile.base.domain.service.*.*(..))")
    public void printLog() {
    }

    @Pointcut("execution(* com.autel.cloud.pile.base.domain.service.*.*(..))")
    public void servicePointcut() {
    }

    @Pointcut("execution(* com.autel.cloud.pile.base.controller.*.*(..))")
    public void controllerPointcut() {
    }

    @Pointcut("@annotation(org.springframework.web.bind.annotation.ExceptionHandler)")
    public void exceptionPointcut() {
    }

//    @Before("servicePointcut()")
//    public void beforeService(JoinPoint joinPoint) {
//        LOGGER.info("Class Method   : {}.{}", joinPoint.getSignature().getDeclaringTypeName(), joinPoint.getSignature().getName());
//        getArgs(joinPoint);
//    }
//
    @AfterReturning(value = "exceptionPointcut()", returning = "result")
    public void afterException(Object result) {
        if (result != null) {
            LOGGER.debug("Response Args  : {}", JSON.toJSONString(result));
        }
        LOGGER.debug("=========================================== End ===========================================");
    }


    /**
     * 环绕通知
     */
    @Around("controllerPointcut()")
    private Object doAround1(ProceedingJoinPoint pjp) throws Throwable {
        long startTime = System.currentTimeMillis();

        if (pjp == null) {
            LOGGER.error("切面拦截失败,ProceedingJoinPoint为空");
            return null;
        }
        ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = attributes.getRequest();

        // 打印请求相关参数
        LOGGER.debug("========================================== Start ==========================================");
        LOGGER.debug("URL            : {}", request.getRequestURL().toString());
        LOGGER.debug("HTTP Method    : {}", request.getMethod());
        LOGGER.debug("Request args    ");
        if (StringUtils.isNotBlank(request.getHeader("language"))) {
            LOGGER.debug("language       : {}", request.getHeader("language"));
        }
        if (StringUtils.isNotBlank(request.getHeader("make"))) {
            LOGGER.debug("make           : {}", request.getHeader("make"));
        }
        LOGGER.debug("Class Method   : {}.{}", pjp.getSignature().getDeclaringTypeName(), pjp.getSignature().getName());
        getArgs(pjp);
        try {
            Object result = pjp.proceed();

            // 出参统一处理
            customizedDateTimeHandle( pjp, result);

            if (result != null) {
                LOGGER.info("Response Args  : {}", JSON.toJSONString(result));
            }
            //耗时
            LOGGER.debug("Time-Consuming : {} ms", System.currentTimeMillis() - startTime);

            LOGGER.debug("=========================================== End ===========================================");
            return result;
        } catch (Exception e) {
            LOGGER.error(request.getMethod() + request.getRequestURL().toString() + pjp.getSignature().getDeclaringTypeName() +"."+ pjp.getSignature().getName(), e);
            throw e;
        }
    }

    /**
     * description: customizedDateTimeHandle 自定义日期、时间格式处理
     * version: 1.0
     * date: 2024/3/25 11:15
     * author: A23204
     *
     * @param pjp
     * @param result
     * @return void
     */
    private void customizedDateTimeHandle(ProceedingJoinPoint pjp, Object result) throws IllegalAccessException {
        // 1. 判断是不是需要处理
        MethodSignature signature = (MethodSignature)pjp.getSignature();
        Method method = signature.getMethod();
        if (!method.isAnnotationPresent(CustomizedDateTime.class)) {
            return;
        }

        UserDateTimePattern userDefaultPattern = getUserDefaultPattern();
        if (userDefaultPattern == null) {
            LOGGER.info("customizedDateTimeHandle, getUserDefaultPattern return null, can not customized date time.");
            return;
        }
        String timePattern = userDefaultPattern.getTimePattern().startsWith("HH") ? "24" : "12";
        userDefaultPattern.setTimePattern(timePattern);

        // 处理返回值的时间属性信息
        if(result instanceof Result || result instanceof R){
            Object data = null;
            if(result instanceof Result ){
                Result ret = (Result) result;
                data = ret.getData();
            } else if (result instanceof R){
                R ret = (R) result;
                data = ret.getData();
            }

            if(null == data){
                return;
            }

            if(data instanceof Page || data instanceof PageResponseDTO){
                List<Object> records;
                if (data instanceof Page) {
                    records = ((Page<Object>) data).getRecords();
                } else {
                    records = ((PageResponseDTO<Object>) data).getRecords();
                }
                if(CollectionUtils.isEmpty(records)){
                    return;
                }
                for (int i = 0; i < records.size(); i++) {
                    Object item = records.get(i);
                    handleUnifiedDateFormatItem(item, userDefaultPattern);
                }
            } else if (data instanceof PageForBillVO || data instanceof IPageVO) {
                List<Object> records;
                if (data instanceof PageForBillVO) {
                    records = ((PageForBillVO<Object>) data).getContent();
                } else {
                    records = ((IPageVO<Object>) data).getContent();
                }
                if(CollectionUtils.isEmpty(records)){
                    return;
                }
                for (int i = 0; i < records.size(); i++) {
                    Object item = records.get(i);
                    handleUnifiedDateFormatItem(item, userDefaultPattern);
                }
            } else if (data instanceof List){
                List dataList = (List) data;
                for (int i = 0; i < dataList.size(); i++) {
                    Object item = dataList.get(i);
                    handleUnifiedDateFormatItem(item, userDefaultPattern);
                }
            }else{
                handleUnifiedDateFormatItem(data, userDefaultPattern);
            }
        }
    }

    private void handleUnifiedDateFormatItem(Object data, UserDateTimePattern userDefaultPattern) throws IllegalAccessException {
        Class<?> dataClass = data.getClass();
        List<Field> fields = Arrays.asList(dataClass.getDeclaredFields());
        List<Field> result = new ArrayList<>();
        if(data.getClass().getSuperclass() != null){
            Class<?> superclass = data.getClass().getSuperclass();
            List<Field> SuperFields = Arrays.asList(superclass.getDeclaredFields());
            result.addAll(SuperFields);
        }
        result.addAll(fields);
        for (int i = 0; i < result.size(); i++) {
            Field field = result.get(i);
            field.setAccessible(Boolean.TRUE);
            if(!field.isAnnotationPresent(FieldPattern.class)){
                continue;
            }
            FieldPattern annotation = field.getAnnotation(FieldPattern.class);
            if(null == annotation){
                continue;
            }
            // 如果属性值是空,直接返回
            Object fieldValue = field.get(data);
            // TODO: 2024/3/11 在此处扩展时间格式类型
            if (fieldValue instanceof  String  && (StringUtils.isBlank(String.valueOf(fieldValue)) || "--".equals(fieldValue))){
                continue;
            }

            int type = annotation.type();
            String format = null;
            if(fieldValue instanceof String && type == Constants.NUMBER_2){
                format = DateUtil.hhmmssTo((String) fieldValue, "24", userDefaultPattern.getTimePattern());
                field.set(data,format);
            } else if(fieldValue instanceof String && type == Constants.NUMBER_1){
                format = DateUtil.dateTimeByLengthTo((String) fieldValue, userDefaultPattern.getDatePattern(), userDefaultPattern.getTimePattern());
                field.set(data,format);
            } else if(fieldValue instanceof List &&  type == Constants.NUMBER_1){
                ((List<?>) fieldValue).forEach(value -> {
                    try {
                        handleUnifiedDateFormatItem(value, userDefaultPattern);
                    } catch (IllegalAccessException e) {
                        LOGGER.error("handle List fieldValue error.");
                    }
                });
            }
        }
    }

    private UserDateTimePattern getUserDefaultPattern() {
        ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (attributes != null) {
            String userInfo = attributes.getRequest().getHeader("user-info");
            if (userInfo == null) {
                return null;
            }
        }

        if (LoginUserHolder.getLoginUser() != null && LoginUserHolder.getLoginUser().getPayload() != null && LoginUserHolder.getLoginUser().getPayload().getUserId() != null && LoginUserHolder.getLoginUser().getPayload().getSellerId() != null && LoginUserHolder.getLoginUser().getPayload().getSellerId() != 0) {
            if(ObjectUtils.isEmpty(pileUserFeign)){
                LOGGER.info("The object pileUserFeign is empty");
                return  null;
            }
            Result<UserDateTimePattern> userDateTimePatternResult = pileUserFeign.queryUserDefaultPattern();
            if (userDateTimePatternResult.getData() != null) {
                return userDateTimePatternResult.getData();
            }
        }
        return null;
    }

    private void getArgs(JoinPoint pjp) {
        Object[] args = pjp.getArgs();
        int index = 1;
        for (Object arg : args) {
            try {
                if (arg instanceof InputStreamSource || arg instanceof AutoCloseable) {
                    LOGGER.debug("参数{}：{}", index, "is I/O");
                } else {
                    String string = JSON.toJSONString(arg);
                    LOGGER.debug("请求参数{}：{}", index, string);
                }
            } catch (Exception e) {
                LOGGER.debug("参数{}：{}", index, arg.toString());
            } finally {
                index++;
            }
        }

    }
}