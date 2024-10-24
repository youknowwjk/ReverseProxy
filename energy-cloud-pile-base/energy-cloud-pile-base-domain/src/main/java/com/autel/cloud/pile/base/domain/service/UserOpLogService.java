package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.infrastructure.sysconfig.log.dto.UserOpLogDTO;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author A22203
 * @Description
 * @Date 2022/5/17 16:34
 */
public interface UserOpLogService {

    PageVO<UserOpLogDTO> queryPages(UserOpLogDTO requestDto);

    void export(UserOpLogDTO page, HttpServletRequest request, HttpServletResponse response);

    Integer save(UserOpLogDTO userOpLogDto);
}
