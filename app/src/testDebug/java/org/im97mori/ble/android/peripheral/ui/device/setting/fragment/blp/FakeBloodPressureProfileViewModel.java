package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;

@HiltViewModel
public class FakeBloodPressureProfileViewModel extends BloodPressureProfileViewModel {

    public String mGetModuleDataString;

    @Inject
    public FakeBloodPressureProfileViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull DeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        super(savedStateHandle, deviceSettingRepository, gson);
    }

    @Nullable
    @Override
    public String getModuleDataString() {
        if (mGetModuleDataString == null) {
            return super.getModuleDataString();
        } else {
            return mGetModuleDataString;
        }
    }


}
