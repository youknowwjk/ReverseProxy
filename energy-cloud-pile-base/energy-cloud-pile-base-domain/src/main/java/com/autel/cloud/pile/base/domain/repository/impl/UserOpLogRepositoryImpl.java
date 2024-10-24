package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.infrastructure.sysconfig.log.dto.UserOpLogDTO;
import com.autel.cloud.infrastructure.sysconfig.log.entity.UserOpLogEntity;
import com.autel.cloud.infrastructure.sysconfig.log.pojo.LogFieldParam;
import com.autel.cloud.infrastructure.sysconfig.log.pojo.LogFieldParams;
import com.autel.cloud.pile.base.domain.repository.UserOpLogRepository;
import com.autel.cloud.pile.base.dto.UserInfoDTO;
import com.autel.cloud.pile.base.infrastructure.feign.UserInfoFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.UserOpLogMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.NoSuchMessageException;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.text.MessageFormat;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author A22203
 * @Description
 * @Date 2022/5/17 19:52
 */
@Slf4j
@Component
public class UserOpLogRepositoryImpl extends ServiceImpl<UserOpLogMapper, UserOpLogEntity> implements UserOpLogRepository {

    @Resource
    UserOpLogMapper tbUserOpLogMapper;

    @Autowired
    UserInfoFeign userInfoFeign;

    @Autowired
    private MessageSource messageSource;

    /**
     * 返回分页的Dto
     *
     * @param dto
     * @return
     */
    @Override
    public PageVO<UserOpLogDTO> selectUserOpLogPage(UserOpLogDTO dto) {
        PageVO<UserOpLogDTO> responsePages = new PageVO<>();
        responsePages.setPage(0);
        responsePages.setPageSize(0);
        // 返回分页Entity
        com.baomidou.mybatisplus.extension.plugins.pagination
                .Page<UserOpLogEntity> entityPage = getTbUserOpLogEntityPage(dto);
        List<UserOpLogEntity> records = entityPage.getRecords();
        if (CollectionUtils.isEmpty(records)) {
            return responsePages;
        }
        List<UserOpLogDTO> dtoList = getUserOpLogDto(records);
        responsePages.setContent(dtoList);
        responsePages.setTotalPages(entityPage.getPages());
        responsePages.setTotalRows(entityPage.getTotal());
        return responsePages;
    }

    /**
     * logEntity转logDto
     *
     * @param logEntities
     * @return
     */
    private List<UserOpLogDTO> getUserOpLogDto(List<UserOpLogEntity> logEntities) {
        Set<Long> userIds = logEntities.stream()
                .filter(r -> r.getOperId() != null && r.getOperId() != 0)
                .map(UserOpLogEntity::getOperId)
                .collect(Collectors.toSet());
        Map<Long, UserInfoDTO> userInfoMap = getUserInfoMap(userIds);
        log.info("获取到的用户数量:{} Map:{}", userInfoMap.size(),
                JSON.toJSONString(userInfoMap));
        List<UserOpLogDTO> dtoList = Lists.newArrayList();
        logEntities.forEach(entity -> {
            UserOpLogDTO logDTO = new UserOpLogDTO();
            dtoList.add(logDTO);
            BeanUtils.copyProperties(entity, logDTO);
            logDTO.setJsonResult(entity.getJsonResult());
            // 设置操作人名称
            setOperName(userInfoMap, entity, logDTO);
            // 设置Title
            setLogDtoTitle(logDTO);
            // 设置Detail
            setLogDtoDetail(logDTO);
        });
        return dtoList;
    }

    private void setOperName(Map<Long, UserInfoDTO> userInfoMap, UserOpLogEntity entity, UserOpLogDTO logDTO) {
        Long operId = entity.getOperId();
        UserInfoDTO userInfo = userInfoMap.get(operId);
        log.info("用户Id：{} 获取到用户信息：{}", operId, JSON.toJSONString(userInfo));
        if (userInfo != null && StringUtils.isNotBlank(userInfo.getUsername())) {
            String username = userInfo.getUsername();
            logDTO.setOperName(username);
            return;
        }
        logDTO.setOperName(logDTO.getOperId() + "");
    }

    // 设置Title
    private void setLogDtoTitle(UserOpLogDTO logDTO) {
        String titleCode = logDTO.getTitleCode();
        if (StringUtils.isNotBlank(titleCode)) {
            String titleMessage = getMessageSource(titleCode);
            if (StringUtils.isNotBlank(titleMessage)) {
                logDTO.setTitle(titleMessage);
            }
        } else {
            log.info("title：{} 国际化code为空设置title内容为title", logDTO.getTitle());
        }
    }

    /**
     * 设置Detail
     * 顺序：
     * 1、getLogFieldParam
     * 2、titlecode
     * 3、请求参数 OperParam
     *
     * @param logDTO
     */
    private void setLogDtoDetail(UserOpLogDTO logDTO) {
        String logFieldParamStr = logDTO.getLogFieldParam();
        // 设置字段注解参数进入detail
        if (setLogFieldParamToDetail(logDTO, logFieldParamStr)) {
            log.info("设置字段注解参数进入detail：{}", JSON.toJSONString(logFieldParamStr));
            return;
        }
        // 设置title国际化参数进入detail
        if (setTitleMessageToDetail(logDTO)) {
            log.info("设置title国际化参数进入detail");
            return;
        }
        logDTO.setDetail(logDTO.getOperParam());
        log.info("设置请求参数进入detail");
    }

    /**
     * 设置title国际化参数进入detail
     *
     * @param logDTO
     * @return
     */
    private boolean setTitleMessageToDetail(UserOpLogDTO logDTO) {
        String titleMessage = getMessageSource(logDTO.getTitleCode());
        if (StringUtils.isNotBlank(titleMessage)) {
            logDTO.setDetail(getMessageSource(logDTO.getTitleCode()));
            log.info("设置title国际化参数进入detail");
            return true;
        }
        return false;
    }

    /**
     * 设置字段注解参数进入detail
     *
     * @param logDTO
     * @param logFieldParamStr
     * @return
     */
    private boolean setLogFieldParamToDetail(UserOpLogDTO logDTO, String logFieldParamStr) {
        if (StringUtils.isNotBlank(logFieldParamStr)) {
            StringBuilder builder = new StringBuilder();
            LogFieldParams logFieldParams = JSON.parseObject(logFieldParamStr, LogFieldParams.class);
            List<LogFieldParam> coll = logFieldParams.getLogFieldParamColl();
            if (CollectionUtils.isNotEmpty(coll)) {
                for (LogFieldParam logFieldParam : coll) {
                    if (validatorLogFieldParam(logFieldParam)) {
                        continue;
                    }
                    builder.append(getMessageSource(logFieldParam.getLogField().getCode()))
                            .append(" : ")
                            .append(logFieldParam.getFieldMap().getValue())
                            .append("  ");
                }
                String detail = builder.toString();
                if (StringUtils.isNotBlank(detail)) {
                    log.info("设置字段注解参数进入detail");
                    logDTO.setDetail(detail);
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * 检验LogFieldParam
     *
     * @param logFieldParam
     * @return
     */
    private boolean validatorLogFieldParam(LogFieldParam logFieldParam) {
        if (logFieldParam == null) {
            return true;
        }
        if (logFieldParam.getFieldMap() != null) {
            Object value = logFieldParam.getFieldMap().getValue();
            if (value == null || StringUtils.isBlank(value + "")) {
                log.info("日志切面字段：{} 未获取到字段值",
                        logFieldParam.getApiModelProperty().getValue());
                return true;
            }
        }
        if (logFieldParam.getLogField() != null) {
            String code = logFieldParam.getLogField().getCode();
            if (StringUtils.isBlank(code)) {
                log.info("日志切面字段：{} 获取不到国际化code",
                        logFieldParam.getApiModelProperty().getValue());
                return true;
            }
            String describe = getMessageSource(code);
            if (StringUtils.isBlank(describe)) {
                log.info("日志切面字段：{} 国际化code：{} 没有国际化翻译值",
                        logFieldParam.getLogField().getCode(),
                        logFieldParam.getApiModelProperty().getValue());
                return true;
            }
        }
        return false;
    }

    public String getMessageSource(String name) {
        log.info("国际化配置 {}", JSON.toJSON(messageSource));
        try {
            return messageSource.getMessage(name, null, LocaleContextHolder.getLocale());
        } catch (NoSuchMessageException e) {
            log.error(MessageFormat.format("获取国际化配置异常，异常信息:{0}",e));
        }
        return null;
    }

    private Map<Long, UserInfoDTO> getUserInfoMap(Set<Long> userIds) {
        List<Long> list = Lists.newArrayList();
        list.addAll(userIds);
        Map<Long, UserInfoDTO> userNameMap = Maps.newHashMap();
        Result<List<UserInfoDTO>> userInfoResult = userInfoFeign.getUserNameList(list);
        if (userInfoResult.getCode() != 200 || userInfoResult.getData() == null) {
            return userNameMap;
        }
        for (UserInfoDTO infoDTO : userInfoResult.getData()) {
            userNameMap.put(infoDTO.getId(), infoDTO);
        }
        return userNameMap;
    }

    /**
     * 返回分页Entity
     *
     * @param requestDto
     * @return
     */
    public com.baomidou.mybatisplus.extension.plugins.pagination.Page<UserOpLogEntity> getTbUserOpLogEntityPage(UserOpLogDTO requestDto) {
        com.baomidou.mybatisplus.extension.plugins.pagination.Page<UserOpLogEntity> entityPage
                = new com.baomidou.mybatisplus.extension.plugins.pagination.Page<>(requestDto.getPage(), requestDto.getPageSize());
        LambdaQueryWrapper<UserOpLogEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.ge(requestDto.getStartTime() != null, UserOpLogEntity::getCreateTime, requestDto.getStartTime());
        queryWrapper.le(requestDto.getEndTime() != null, UserOpLogEntity::getCreateTime, requestDto.getEndTime());
        queryWrapper.eq(requestDto.getBusinessType() != null, UserOpLogEntity::getBusinessType, requestDto.getBusinessType());
        Long sellerId = UserUtil.getSellerId();
        queryWrapper.eq(sellerId != null, UserOpLogEntity::getSellerId, sellerId);
        queryWrapper.orderByDesc(UserOpLogEntity::getCreateTime);
        tbUserOpLogMapper.selectPage(entityPage, queryWrapper);
        return entityPage;
    }

    @Override
    public Integer save(UserOpLogDTO userOpLogDto) {
        UserOpLogEntity entity = new UserOpLogEntity();
        BeanUtils.copyProperties(userOpLogDto, entity);
        return tbUserOpLogMapper.insert(entity);
    }
}
