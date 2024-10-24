package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.domain.repository.MailboxConfigRepository;
import com.autel.cloud.pile.base.domain.service.MailboxConfigService;
import com.autel.cloud.pile.base.dto.MailboxConfigDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbMailboxConfigEntity;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@Log4j2
public class MailboxConfigServiceImpl implements MailboxConfigService {
    private final MailboxConfigRepository mailboxConfigRepository;

    public MailboxConfigServiceImpl(MailboxConfigRepository mailboxConfigRepository) {
        this.mailboxConfigRepository = mailboxConfigRepository;
    }

    @Override
    public Boolean addMailbox(MailboxConfigDTO mailboxConfigDTO) {
        return mailboxConfigRepository.addMailbox(mailboxConfigDTO);
    }

    @Override
    public Boolean updateMailbox(MailboxConfigDTO mailboxConfigDTO) {
        return mailboxConfigRepository.updateMailbox(mailboxConfigDTO);
    }

    @Override
    public Boolean delMailbox(Long id) {
        return mailboxConfigRepository.delMailbox(id);
    }

    @Override
    public PageVO<MailboxConfigDTO> selectMailboxPage(MailboxConfigDTO mailboxConfigDTO) {
        return mailboxConfigRepository.selectMailboxPage(mailboxConfigDTO);
    }

    @Override
    public MailboxConfigDTO selectMailboxByOperatorId(Long operatorId) {
        TbMailboxConfigEntity tbMailboxConfigEntity = mailboxConfigRepository.selectMailboxByOperatorId(operatorId);
        if (null != tbMailboxConfigEntity) {
            MailboxConfigDTO mailboxConfigDTO = new MailboxConfigDTO();
            BeanUtil.copyProperties(tbMailboxConfigEntity, mailboxConfigDTO);
            return mailboxConfigDTO;
        }
        return null;
    }
}
