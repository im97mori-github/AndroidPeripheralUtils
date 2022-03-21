package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import androidx.annotation.NonNull;
import androidx.lifecycle.SavedStateHandle;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.subjects.PublishSubject;

@HiltViewModel
public class FakeBloodPressureProfileViewModel extends BloodPressureProfileViewModel {

    private final SavedStateHandle mSavedStateHandle;

    public final PublishSubject<byte[]> mObserveSaveSubject = PublishSubject.create();

    @Inject
    public FakeBloodPressureProfileViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull DeviceSettingRepository deviceSettingRepository) {
        super(savedStateHandle, deviceSettingRepository);
        mSavedStateHandle = savedStateHandle;
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSaveSubject.subscribe(s -> mSavedStateHandle.set("KEY_SAVED_DATA", s), onError));
    }

}
