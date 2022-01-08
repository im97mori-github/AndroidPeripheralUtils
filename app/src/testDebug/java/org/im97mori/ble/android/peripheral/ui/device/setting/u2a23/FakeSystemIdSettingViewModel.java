package org.im97mori.ble.android.peripheral.ui.device.setting.u2a23;

import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.processors.PublishProcessor;

@HiltViewModel
public class FakeSystemIdSettingViewModel extends SystemIdSettingViewModel {

    public final PublishProcessor<String> mObserveSetupProcessor = PublishProcessor.create();

    public final FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private final SavedStateHandle mSavedStateHandle;

    public final PublishProcessor<Intent> mObserveSaveProcessor = PublishProcessor.create();

    public java.util.function.Consumer<Boolean> mUpdateIsErrorResponseConsumer;
    public java.util.function.Consumer<String> mUpdateManufacturerIdentifierConsumer;
    public java.util.function.Consumer<String> mUpdateOrganizationallyUniqueIdentifierConsumer;
    public java.util.function.Consumer<String> mUpdateResponseCodeConsumer;
    public java.util.function.Consumer<String> mUpdateResponseDelayConsumer;

    @Inject
    FakeSystemIdSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull FakeDeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        super(savedStateHandle, deviceSettingRepository, gson);
        mSavedStateHandle = savedStateHandle;
        mFakeDeviceSettingRepository = deviceSettingRepository;
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSetupProcessor
                .subscribe(s -> mDisposable.add(Single.<String>create(emitter -> emitter.onSuccess(s))
                        .flatMapCompletable(t -> {
                            switch (t) {
                                case "test_isErrorResponse_00002":
                                    test_isErrorResponse_00002();
                                    break;
                                case "test_isErrorResponse_00003":
                                    test_isErrorResponse_00003();
                                    break;
                                case "test_responseCode_visibility_00001":
                                    test_responseCode_visibility_00001();
                                    break;
                                case "test_responseCode_visibility_00002":
                                    test_responseCode_visibility_00002();
                                    break;
                                case "test_responseCode_00001":
                                    test_responseCode_00001();
                                    break;
                                case "test_responseCode_error_00002":
                                    test_responseCode_error_00002();
                                    break;
                                case "test_responseDelay_00001":
                                    test_responseDelay_00001();
                                    break;
                                case "test_responseDelay_error_00002":
                                    test_responseDelay_error_00002();
                                    break;
                                case "test_menu_save_00002":
                                    test_menu_save_00002();
                                    break;
                                case "test_updateResponseCode_00001":
                                    test_updateResponseCode_00001();
                                    break;
                                case "test_manufacturerIdentifier_visibility_00001":
                                    test_manufacturerIdentifier_visibility_00001();
                                    break;
                                case "test_manufacturerIdentifier_visibility_00002":
                                    test_manufacturerIdentifier_visibility_00002();
                                    break;
                                case "test_manufacturerIdentifier_error_00002":
                                    test_manufacturerIdentifier_error_00002();
                                    break;
                                case "test_updateManufacturerIdentifier_00001":
                                    test_updateManufacturerIdentifier_00001();
                                    break;
                                case "test_organizationallyUniqueIdentifier_visibility_00001":
                                    test_organizationallyUniqueIdentifier_visibility_00001();
                                    break;
                                case "test_organizationallyUniqueIdentifier_visibility_00002":
                                    test_organizationallyUniqueIdentifier_visibility_00002();
                                    break;
                                case "test_organizationallyUniqueIdentifier_error_00002":
                                    test_organizationallyUniqueIdentifier_error_00002();
                                    break;
                                case "test_updateOrganizationallyUniqueIdentifier_00001":
                                    test_updateOrganizationallyUniqueIdentifier_00001();
                                    break;
                                default:
                            }
                            return Completable.complete();
                        }).subscribe(onComplete, onError))));
    }

    @Override
    public void updateIsErrorResponse(boolean checked) {
        if (mUpdateIsErrorResponseConsumer != null) {
            mUpdateIsErrorResponseConsumer.accept(checked);
        }
        super.updateIsErrorResponse(checked);
    }

    @Override
    public void updateManufacturerIdentifier(@NonNull String text) {
        if (mUpdateManufacturerIdentifierConsumer != null) {
            mUpdateManufacturerIdentifierConsumer.accept(text);
        }
        super.updateManufacturerIdentifier(text);
    }

    @Override
    public void updateOrganizationallyUniqueIdentifier(@NonNull String text) {
        if (mUpdateOrganizationallyUniqueIdentifierConsumer != null) {
            mUpdateOrganizationallyUniqueIdentifierConsumer.accept(text);
        }
        super.updateOrganizationallyUniqueIdentifier(text);
    }

    @Override
    public void updateResponseCode(@NonNull String text) {
        if (mUpdateResponseCodeConsumer != null) {
            mUpdateResponseCodeConsumer.accept(text);
        }
        super.updateResponseCode(text);
    }

    @Override
    public void updateResponseDelay(@NonNull String text) {
        if (mUpdateResponseDelayConsumer != null) {
            mUpdateResponseDelayConsumer.accept(text);
        }
        super.updateResponseDelay(text);
    }

    @Override
    public void observeSave(@NonNull Consumer<Intent> onSuccess, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSaveProcessor.subscribe(onSuccess, onError));
    }

    private void test_isErrorResponse_00002() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", false);
    }

    private void test_isErrorResponse_00003() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", true);
    }

    private void test_responseCode_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", false);
    }

    private void test_responseCode_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", true);
    }

    private void test_responseCode_00001() {
        mSavedStateHandle.set("KEY_RESPONSE_CODE", "1");
    }

    private void test_responseCode_error_00002() {
        mSavedStateHandle.set("KEY_RESPONSE_CODE", "");
    }

    private void test_responseDelay_00001() {
        mSavedStateHandle.set("KEY_RESPONSE_DELAY", "1");
    }

    private void test_responseDelay_error_00002() {
        mSavedStateHandle.set("KEY_RESPONSE_DELAY", "");
    }

    private void test_menu_save_00002() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", true);
    }

    private void test_updateResponseCode_00001() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", true);
    }

    private void test_manufacturerIdentifier_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", false);
    }

    private void test_manufacturerIdentifier_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", true);
    }

    private void test_manufacturerIdentifier_error_00002() {
        mSavedStateHandle.set("KEY_MANUFACTURER_IDENTIFIER", "");
    }

    private void test_updateManufacturerIdentifier_00001() {
        mSavedStateHandle.set("KEY_MANUFACTURER_IDENTIFIER", "1");
    }

    private void test_organizationallyUniqueIdentifier_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", false);
    }

    private void test_organizationallyUniqueIdentifier_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", true);
    }

    private void test_organizationallyUniqueIdentifier_error_00002() {
        mSavedStateHandle.set("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER", "");
    }

    private void test_updateOrganizationallyUniqueIdentifier_00001() {
        mSavedStateHandle.set("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER", "1");
    }

}