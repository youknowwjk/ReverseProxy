/*
package com.autel.cloud.pile.base.controller.aop;

import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import lombok.extern.log4j.Log4j2;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.Objects;

*/
/**
 * @ClassName PageAuthCheckServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/6/23 20:22
 * @Version 0.0.1-SNAPSHOT
 *//*

@Service
@Aspect
@Log4j2
public class SellerAccountDataAuthCheck {

    @Pointcut("execution(* com.autel.cloud.pile.base.controller.SellerAccountController.queryStripeAccount(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.SellerAccountController.updateStripeAccount(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.SellerAccountController.unBindStripeAccount(..))" +
            "")
    public void point() {
    }

    @Around("point()")
    public Object around(ProceedingJoinPoint proceedingJoinPoint) {

        String methodName = proceedingJoinPoint.getSignature().getName();
        String className = proceedingJoinPoint.getSignature().getDeclaringTypeName();
        // 如果是oicp服务Feign调用，跳过校验
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        if (jwtInfo == null
                || jwtInfo.getPayload() == null
                || jwtInfo.getPayload().getSellerId() == null) {
            throw new MessageCodeException(PileBaseEnum.NO_DATA_ACCESS);
        }
        try {
            return proceedingJoinPoint.proceed();
        } catch (MessageCodeException e) {
            log.error("SellerAccountDataAuthCheck.around proceed failed and MessageCodeException = ", e);
            throw e;
        } catch (Throwable throwable) {
            log.error("SellerAccountDataAuthCheck.around proceed failed and throwable = ", throwable);
            throw new MessageCodeException(throwable.getMessage());
        }
    }
}
*/
