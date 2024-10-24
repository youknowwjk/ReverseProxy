package com.autel.cloud.pile.base.infrastructure.aop;

import com.alibaba.fastjson.JSON;
import lombok.extern.log4j.Log4j2;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @ClassName StringEscapeAop
 * @Author A22121
 * @Description
 * @Date 2022/9/22 16:33
 * @Version 0.0.1-SNAPSHOT
 */
//@Service
//@Aspect
@Log4j2
public class StringEscapeAop {
    @Pointcut("execution(* com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic.*(..))" +
            "|| execution(* com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic.*(..))" +
            "")
    public void point() {
    }

    private static final List<String> METHOD_NAME_LIST_FOR_ONE = Arrays.asList("findAllByPileSn",
            "findByPileSnLike", "findByPileSn", "findByPileSnNotAndLocationId"
    );

    private static final List<String> METHOD_NAME_LIST_FOR_TWO = Arrays.asList("findAllByLocationIdAndPileSn",
            "findByLocationIdAndPileSnLike"
    );

    private static final List<String> METHOD_NAME_LIST_FOR_ONE_LIST = Arrays.asList("findAllByPileSnIn",
            "findByPileSnIn"
            , "findByPileSnInAndLocationId"
            , "findByPileSnInAndLocationIdNot"
    );

    @Around("point()")
    public Object around(ProceedingJoinPoint proceedingJoinPoint) {
        try {
            String className = proceedingJoinPoint.getSignature().getDeclaringTypeName();
            String methodName = proceedingJoinPoint.getSignature().getName();
            log.info("StringEscapeAop around className = " + className + " and methodName = " + methodName);
            if (METHOD_NAME_LIST_FOR_ONE.contains(methodName)) {
                String pileSn = proceedingJoinPoint.getArgs()[0].toString();
                log.info("StringEscapeAop around and pileSn front = " + pileSn);
                proceedingJoinPoint.getArgs()[0] = QueryParserBase.escape(pileSn);
                log.info("StringEscapeAop around and pileSn after = " + proceedingJoinPoint.getArgs()[0]);
            } else if (METHOD_NAME_LIST_FOR_TWO.contains(methodName)) {
                String pileSn = proceedingJoinPoint.getArgs()[1].toString();
                proceedingJoinPoint.getArgs()[1] = QueryParserBase.escape(pileSn);
            } else if (METHOD_NAME_LIST_FOR_ONE_LIST.contains(methodName)) {
                List<String> pileSnList = JSON.parseArray(proceedingJoinPoint.getArgs()[0].toString(), String.class);
                proceedingJoinPoint.getArgs()[0] = pileSnList.stream().map(QueryParserBase::escape).collect(Collectors.toList());
            }
            try {
                return proceedingJoinPoint.proceed();
            } catch (Throwable throwable) {
                log.error("StringEscapeAop.around throwable = ", throwable);
            }
            return null;
        } catch (Exception e) {
            log.error("StringEscapeAop.around exception = ", e);
            return null;
        }
    }
}
