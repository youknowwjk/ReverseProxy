package com.autel.cloud.pile.base.controller.aop;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.pile.base.domain.service.LocationMeterService;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import java.util.Collections;
import lombok.extern.log4j.Log4j2;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Objects;

/**
 * @ClassName PageAuthCheckServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/6/23 20:22
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Aspect
@Log4j2
public class MeterDataAuthCheck {
    @Resource
    private LocationMeterService locationMeterService;

    @Pointcut("execution(* com.autel.cloud.pile.base.controller.LocationMeterController.delete(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.LocationMeterController.detail(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.LocationMeterController.update(..))" +
            "")
    public void point() {
    }

    @Around("point()")
    public Object around(ProceedingJoinPoint proceedingJoinPoint) {

        String methodName = proceedingJoinPoint.getSignature().getName();
        String className = proceedingJoinPoint.getSignature().getDeclaringTypeName();
        // 如果是oicp服务Feign调用，跳过校验
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String oicpRequest = "oicpRequest";
        String suffix = request.getHeader("oicpRequest");

        boolean isCheckAuth = Boolean.FALSE;
        if ("detail".equals(methodName)) {
            long id = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = this.checkAuthId(id);
        } else if ("update".equals(methodName)) {
            JSONObject jsonObject = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long id = jsonObject.getLong("id");
            isCheckAuth = this.checkAuthId(id);
        } else if ("delete".equals(methodName)) {
            long id = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = this.checkAuthId(id);
        }
        // 放行oicp feign 接口调用
        if (StringUtils.endsWithIgnoreCase(oicpRequest, suffix)) {
            isCheckAuth = Boolean.TRUE;
        }

        // 商家id为0则全部放开
        if (!isCheckAuth) {
            try {
                if (LoginUserHolder.getLoginUser().getPayload().getSellerId().longValue() == 0L) {
                    isCheckAuth = Boolean.TRUE;
                }
            } catch (Exception e) {
                isCheckAuth = Boolean.FALSE;
            }
        }

        // 判断用户是否有权限
        if (!isCheckAuth) {
            throw new MessageCodeException(PileBaseEnum.NO_DATA_ACCESS);
        }
        try {
            return proceedingJoinPoint.proceed();
        } catch (MessageCodeException e) {
            log.error("MeterDataAuthCheck.around proceed failed and MessageCodeException = ", e);
            throw e;
        } catch (Throwable throwable) {
            log.error("MeterDataAuthCheck.around proceed failed and throwable = ", throwable);
            throw new MessageCodeException(throwable.getMessage());
        }
    }

    private List<Long> getIdList() {
        try {
            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
            if (jwtInfo == null
                    || jwtInfo.getPayload() == null
                    || jwtInfo.getPayload().getSellerId() == null) {
                return Collections.emptyList();
            }
            return locationMeterService.selectIdListBySellerId(jwtInfo.getPayload().getSellerId());
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    private boolean checkAuthId(Long id) {
        try {
            return this.getIdList().contains(id);
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }
}
